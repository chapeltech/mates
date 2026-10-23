{-# LANGUAGE OverloadedStrings #-}

module Mates.Server.DAV
  ( davApplication
  , davApplicationFor
  )
where

import qualified Mates.Digest as Digest
import qualified Mates.Server.Protocol as Protocol
import Mates.Server.Condition (checkIf)
import Mates.Server.Security (scopedStore, mayAccess)
import Mates.Server.Database (Collection (..))
import qualified Data.Text.Encoding as TE
import qualified Network.Wai as Wai
import Control.Exception (try, evaluate, catch)
import Mates.CalendarNative (CalendarFailure (..))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Mates.Server.ClientProfile
  ( ClientProfile
  , clientProfileName
  , recognizeClient
  )
import Mates.Server.Render
  ( RenderedPayload (..)
  , renderContactVCardFromRaw
  , renderDavPayload
  )
import Mates.Server.Store (Store (..), DavConflict (..))
import Mates.Server.Types (DavObject (..), DavPayload (..))
import Network.HTTP.Types
  ( Header
  , HeaderName
  , hContentType
  , mkStatus
  , status200
  , status201
  , status204
  , status400
  , status404
  , status412
  , status501
  )
import Network.Wai
  ( Application
  , Request
  , Response
  , pathInfo
  , requestHeaders
  , requestMethod
  , responseLBS
  )

davApplication :: Store -> Application
davApplication = davApplicationFor Nothing "http://127.0.0.1:8080"

davApplicationFor :: Maybe Text -> Text -> Store -> Application
davApplicationFor principal origin store request respond = do
  bodyResult <- readBoundedBody request
  case bodyResult of
    Left response -> respond response
    Right body -> handleRequest body
 where
  handleRequest body = do
    let writing = requestMethod request `elem` ["PUT", "DELETE", "PROPPATCH"]
        userAgent = maybe "" (TE.decodeUtf8With lenientDecode) (lookup "User-Agent" (requestHeaders request))
        segments = normalizedDavPath (pathInfo request)
        href = hrefFromSegments segments
        validPath = all (\segment -> segment /= "." && segment /= ".." && not (Text.any (`elem` ['/', '\\', '\0', '%']) segment)) segments
    result <- try ((storeTransaction store principal userAgent writing $ \transactionStore -> do
      storePublicOrigin transactionStore origin
      enabled <- maybe (pure True) (storePrincipalEnabled transactionStore) principal
      allowed <- storeCollections transactionStore principal
      let home = null segments || segments == ["calendars"] || take 1 segments == ["principals"]
          allowedHere = home && not writing || mayAccess allowed writing href || mayAccess allowed writing (href <> "/")
      if not validPath then pure (responseLBS status400 [] "Invalid DAV path")
      else if not enabled || not allowedHere then pure (responseLBS (mkStatus 403 "Forbidden") [] "Forbidden")
      else do
        let scoped = scopedStore allowed transactionStore
            profile = recognizeClient (requestHeaders request)
        condition <- case requestHeader "If" request of
          Nothing -> pure (Right True)
          Just value -> checkIf (resourceStates origin profile scoped allowed) (Text.unpack href) (ByteString8.unpack value)
        case condition of
          Left _ -> pure (responseLBS status400 [] "Invalid WebDAV If header")
          Right False -> pure (responseLBS status412 [] "WebDAV If precondition failed")
          Right True -> davResponse principal origin scoped body request pure >>= evaluate) `catch` (\(DavConflict _) -> pure (responseLBS (mkStatus 403 "Forbidden") [("Content-Type","application/xml")] (encodeText ("<D:error xmlns:D=\"DAV:\" xmlns:R=\"" <> (if take 1 segments == ["contacts"] then "urn:ietf:params:xml:ns:carddav" else "urn:ietf:params:xml:ns:caldav") <> "\"><R:no-uid-conflict/></D:error>")))))
    case result of
      Right response -> respond response
      Left (CalendarFailure _) -> respond (responseLBS (mkStatus 403 "Forbidden") [("Content-Type","application/xml")] "<D:error xmlns:D=\"DAV:\" xmlns:CAL=\"urn:ietf:params:xml:ns:caldav\"><CAL:number-of-matches-within-limits/></D:error>")

davResponse :: Maybe Text -> Text -> Store -> LazyByteString.ByteString -> Request -> (Response -> IO Response) -> IO Response
davResponse principal origin store body request respond = do
  let profile = recognizeClient (requestHeaders request)
      segments = normalizedDavPath (pathInfo request)
  case requestMethod request of
    "OPTIONS" ->
      respond (responseLBS status204 davOptionsHeaders "")
    "PROPFIND" -> do
      Protocol.propfind principal store (presentObjects profile store) segments (TE.decodeUtf8With lenientDecode <$> requestHeader "Depth" request) body >>= respond
    "REPORT" -> do
      Protocol.report origin store (presentObjects profile store) segments body >>= respond
    "GET" ->
      respondObjectOrNotFound request profile store segments False respond
    "HEAD" ->
      respondObjectOrNotFound request profile store segments True respond
    "PUT" -> do
      case objectTargetFromSegments segments of
        Just target ->
          putDavObject profile store request target body respond
        Nothing ->
          respond
            ( responseLBS
                status400
                (plainHeaders profile)
                "PUT is only supported below /dav/contacts/ or /dav/calendars/*/\n"
            )
    "DELETE" ->
      case objectTargetFromSegments segments of
        Just target ->
          deleteDavObject profile store request target respond
        Nothing ->
          respond
            ( responseLBS
                status400
                (plainHeaders profile)
                "DELETE is only supported below /dav/contacts/ or /dav/calendars/*/\n"
            )
    _ ->
      respond
        ( responseLBS
            status501
            (plainHeaders profile)
            "DAV method is not implemented yet\n"
        )

data DavObjectTarget
  = ContactObject Text
  | CalendarObject Text

putDavObject ::
  ClientProfile ->
  Store ->
  Request ->
  DavObjectTarget ->
  LazyByteString.ByteString ->
  (Response -> IO Response) ->
  IO Response
putDavObject profile store request target body respond = do
  existing <- lookupPresentedTargetObject profile store target
  case preconditionFailure request existing of
    Just problem ->
      respond (responseLBS status412 (plainHeaders profile) (encodeText problem))
    Nothing -> do
      result <- putTargetObject store target body
      case result of
        Left problem ->
          respond (responseLBS status400 (plainHeaders profile) (encodeText problem))
        Right created -> do
          stored <- lookupPresentedTargetObject profile store target
          let headers = maybe [] (\object -> if davObjectBody object == body then etagHeader object else []) stored <> plainHeaders profile
              status =
                if created then
                  status201

                else
                  status204
          respond (responseLBS status headers "")

deleteDavObject ::
  ClientProfile ->
  Store ->
  Request ->
  DavObjectTarget ->
  (Response -> IO Response) ->
  IO Response
deleteDavObject profile store request target respond = do
  existing <- lookupPresentedTargetObject profile store target
  case preconditionFailure request existing of
    Just problem ->
      respond (responseLBS status412 (plainHeaders profile) (encodeText problem))
    Nothing -> do
      removed <- deleteTargetObject store target
      respond
        ( responseLBS
            (if removed then status204 else status404)
            (plainHeaders profile)
            ""
        )

lookupTargetObject :: Store -> DavObjectTarget -> IO (Maybe DavObject)
lookupTargetObject store target =
  case target of
    ContactObject href -> storeLookupContactObject store href
    CalendarObject href -> storeLookupCalendarObject store href

lookupPresentedTargetObject :: ClientProfile -> Store -> DavObjectTarget -> IO (Maybe DavObject)
lookupPresentedTargetObject profile store target =
  traverse (presentObject profile store) =<< lookupTargetObject store target

presentObjects :: ClientProfile -> Store -> [DavObject] -> IO [DavObject]
presentObjects profile store =
  mapM (presentObject profile store)

presentObject :: ClientProfile -> Store -> DavObject -> IO DavObject
presentObject profile store object
  | "text/vcard" `Text.isPrefixOf` davObjectContentType object = do
      payload <- storeLookupDavObject store (hrefSegments (davObjectHref object))
      case payload of
        Just (DavContact contact) ->
          let rawBody =
                Text.decodeUtf8With lenientDecode (LazyByteString.toStrict (davObjectBody object))
              body =
                LazyByteString.fromStrict
                  (Text.encodeUtf8 (renderContactVCardFromRaw profile rawBody contact))
           in pure
                object
                  { davObjectContentType = "text/vcard; charset=utf-8"
                  , davObjectBody = body
                  , davObjectEtag = renderedEtag body
                  }
        Just value ->
          let rendered = renderDavPayload profile value
           in pure
                object
                  { davObjectContentType = renderedContentType rendered
                  , davObjectBody = renderedBody rendered
                  , davObjectEtag = renderedEtag (renderedBody rendered)
                  }
        Nothing ->
          pure object
  | otherwise =
      pure object

putTargetObject ::
  Store ->
  DavObjectTarget ->
  LazyByteString.ByteString ->
  IO (Either Text Bool)
putTargetObject store target body =
  case target of
    ContactObject href -> storePutContactObject store href body
    CalendarObject href -> storePutCalendarObject store href body

deleteTargetObject :: Store -> DavObjectTarget -> IO Bool
deleteTargetObject store target =
  case target of
    ContactObject href -> storeDeleteContactObject store href
    CalendarObject href -> storeDeleteCalendarObject store href

respondObjectOrNotFound ::
  Request ->
  ClientProfile ->
  Store ->
  [Text] ->
  Bool ->
  (Response -> IO Response) ->
  IO Response
respondObjectOrNotFound request profile store segments headOnly respond =
  case objectTargetFromSegments segments of
    Just target -> do
      found <- lookupPresentedTargetObject profile store target
      case found of
        Just object | maybe False (\value -> etagMatchesWeak value object) (requestHeader "If-None-Match" request) -> respond (responseLBS (mkStatus 304 "Not Modified") (etagHeader object) "")
        Just object ->
          respond
            ( responseLBS
                status200
                (objectHeaders object)
                (if headOnly then "" else davObjectBody object)
            )
        Nothing ->
          respond
            ( responseLBS
                status404
                (plainHeaders profile)
                "DAV object not found\n"
            )
    Nothing ->
      respond
        ( responseLBS status404 (plainHeaders profile) "DAV object not found\n"
        )

requestHeader :: HeaderName -> Request -> Maybe ByteString.ByteString
requestHeader name request =
  lookup name (requestHeaders request)

preconditionFailure :: Request -> Maybe DavObject -> Maybe Text
preconditionFailure request existing
  | Just value <- requestHeader "If-Match" request
  , not (maybe False (etagMatches value) existing) = Just "If-Match precondition failed\n"
  | Just value <- requestHeader "If-None-Match" request
  , maybe False (etagMatchesWeak value) existing = Just "If-None-Match precondition failed\n"
  | otherwise = Nothing

etagMatches :: ByteString.ByteString -> DavObject -> Bool
etagMatches value object = any (\candidate -> candidate == "*" || candidate == davObjectEtag object) (Text.strip <$> Text.splitOn "," (TE.decodeUtf8With lenientDecode value))

contactHrefFromSegments :: [Text] -> Maybe Text
contactHrefFromSegments segments =
  case filter (not . Text.null) segments of
    ["contacts", fileName]
      | not (Text.null fileName) ->
          Just ("/dav/contacts/" <> fileName)
    _ ->
      Nothing

calendarHrefFromSegments :: [Text] -> Maybe Text
calendarHrefFromSegments segments =
  case filter (not . Text.null) segments of
    ["calendars", calendarName, fileName]
      | not (Text.null calendarName) && not (Text.null fileName) ->
          Just ("/dav/calendars/" <> calendarName <> "/" <> fileName)
    _ ->
      Nothing

objectTargetFromSegments :: [Text] -> Maybe DavObjectTarget
objectTargetFromSegments segments =
  case contactHrefFromSegments segments of
    Just href -> Just (ContactObject href)
    Nothing ->
      CalendarObject <$> calendarHrefFromSegments segments

hrefFromSegments :: [Text] -> Text
hrefFromSegments [] =
  "/dav/"
hrefFromSegments segments =
  "/dav/" <> Text.intercalate "/" segments

hrefSegments :: Text -> [Text]
hrefSegments =
  normalizedDavPath . Text.splitOn "/" . normalizeDavHref

normalizedDavPath :: [Text] -> [Text]
normalizedDavPath values = case filter (not . Text.null) values of
  "dav":rest -> normalizedDavPath rest
  cleaned -> cleaned

normalizeDavHref :: Text -> Text
normalizeDavHref href =
  let
    stripped = Text.strip href
    pathOnly =
      case Text.breakOn "://" stripped of
        (_, "") ->
          stripped
        (_, rest) ->
          case Text.dropWhile (/= '/') (Text.drop 3 rest) of
            "" -> "/"
            path -> path
    absolute =
      if "/" `Text.isPrefixOf` pathOnly then
        pathOnly

      else
        "/" <> pathOnly
  in
  absolute

davOptionsHeaders :: [Header]
davOptionsHeaders =
  [ ("Allow", "OPTIONS, PROPFIND, REPORT, GET, HEAD, PUT, DELETE")
  , ("DAV", "1, addressbook, calendar-access, sync-collection")
  , ("MS-Author-Via", "DAV")
  ]

plainHeaders :: ClientProfile -> [Header]
plainHeaders profile =
  (hContentType, "text/plain; charset=utf-8")
    : ("X-Mates-Client-Profile", Text.encodeUtf8 (clientProfileName profile))
    : davOptionsHeaders

objectHeaders :: DavObject -> [Header]
objectHeaders object =
  (hContentType, Text.encodeUtf8 (davObjectContentType object))
    : etagHeader object
      <> [ ( "Content-Length"
           , ByteString8.pack (show (LazyByteString.length (davObjectBody object)))
           )
         , ("Vary", "User-Agent")
         , ("DAV", "1, addressbook, calendar-access")
         ]

etagHeader :: DavObject -> [Header]
etagHeader object =
  [("ETag", Text.encodeUtf8 (davObjectEtag object))]

renderedEtag :: LazyByteString.ByteString -> Text
renderedEtag = Digest.etag

encodeText :: Text -> LazyByteString.ByteString
encodeText =
  LazyByteString.fromStrict . Text.encodeUtf8


readBoundedBody :: Request -> IO (Either Response LazyByteString.ByteString)
readBoundedBody request
  | requestMethod request `notElem` ["PUT", "PROPFIND", "REPORT", "PROPPATCH"] = pure (Right "")
  | otherwise = go 0 []
  where
    go size chunks = do
      chunk <- Wai.getRequestBodyChunk request
      let nextSize = size + ByteString.length chunk
      if nextSize > 10485760 then pure (Left (responseLBS (mkStatus 413 "Content Too Large") [] "DAV request exceeds 10 MiB"))
      else if ByteString.null chunk then pure (Right (LazyByteString.fromChunks (reverse chunks)))
      else go nextSize (chunk:chunks)

resourceStates :: Text -> ClientProfile -> Store -> [Collection] -> String -> IO [String]
resourceStates origin profile store collections uri
  | ("https://" `Text.isPrefixOf` Text.pack uri || "http://" `Text.isPrefixOf` Text.pack uri) && not ((origin <> "/") `Text.isPrefixOf` Text.pack uri) = pure []
  | otherwise = do
    let href = normalizeDavHref (Text.pack uri)
        tokens = [Text.unpack (collectionToken c) | c <- collections, collectionHref c == href]
    case objectTargetFromSegments (hrefSegments href) of
      Nothing -> pure tokens
      Just target -> do
        object <- lookupPresentedTargetObject profile store target
        pure (tokens ++ maybe [] (\o -> [Text.unpack (davObjectEtag o)]) object)

etagMatchesWeak :: ByteString.ByteString -> DavObject -> Bool
etagMatchesWeak value = etagMatches (TE.encodeUtf8 (Text.intercalate "," [maybe candidate id (Text.stripPrefix "W/" candidate) | raw <- Text.splitOn "," (TE.decodeUtf8With lenientDecode value), let candidate = Text.strip raw]))
