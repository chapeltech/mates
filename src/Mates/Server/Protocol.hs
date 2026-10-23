{-# LANGUAGE OverloadedStrings #-}
module Mates.Server.Protocol (propfind, report, namespaces, multistatus, propResponse, objectProperties) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Char (toLower, toTitle)
import qualified Data.Text.Normalize as Normalize
import Control.Exception (throw)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Data.Maybe (fromMaybe, listToMaybe)
import Network.HTTP.Types (mkStatus)
import Network.HTTP.Types.URI (urlEncode, urlDecode)
import qualified Mates.Digest as Digest
import Network.Wai (Response, responseLBS)
import Text.XML (Element (..), Name (..))
import qualified Mates.Server.Xml as X
import Mates.Server.Database (Collection (..), SyncResult (..))
import Mates.Server.Store (Store (..))
import Mates.Server.Types (DavObject (..))
import Mates.VCard (VCardProperty (..), parseVCards, parseProperties, unfoldLines, decodeText)
import qualified Mates.ICalendar as IC
import qualified Mates.CalendarNative as Native

namespaces :: Text
namespaces = " xmlns:D=\"DAV:\" xmlns:C=\"urn:ietf:params:xml:ns:carddav\" xmlns:CAL=\"urn:ietf:params:xml:ns:caldav\" xmlns:CS=\"http://calendarserver.org/ns/\""

xmlResponse :: Int -> Text -> Response
xmlResponse code body = let encoded = TE.encodeUtf8 ("<?xml version=\"1.0\" encoding=\"utf-8\"?>" <> body) in BS.length encoded `seq` responseLBS (mkStatus code "DAV") [("Content-Type", "application/xml; charset=utf-8")] (LBS.fromStrict encoded)

multistatus :: [Text] -> Text -> Response
multistatus responses extra = xmlResponse 207 ("<D:multistatus" <> namespaces <> ">" <> T.concat responses <> extra <> "</D:multistatus>")

tag :: Name -> Text -> Text
tag n inner = "<P:" <> nameLocalName n <> " xmlns:P=\"" <> X.xmlEscape (fromMaybe "" (nameNamespace n)) <> "\">" <> inner <> "</P:" <> nameLocalName n <> ">"

hrefXml :: Text -> Text
hrefXml h = "<D:href>" <> X.xmlEscape (if "/" `T.isPrefixOf` h then T.intercalate "/" (map (TE.decodeUtf8 . urlEncode False . TE.encodeUtf8) (T.splitOn "/" h)) else h) <> "</D:href>"

propResponse :: Text -> [(Name, Text)] -> Maybe [Name] -> Bool -> Text
propResponse href properties selected namesOnly =
  let defaultNames = [n | (n,_) <- properties, namesOnly || n `notElem` [X.name X.dav "sync-token", X.name X.dav "supported-report-set", X.name X.dav "current-user-privilege-set", X.name X.cal "calendar-timezone"]]
      requested = case selected of
        Nothing -> defaultNames
        Just (marker:rest) | marker == X.name X.dav "allprop" -> defaultNames ++ rest
        Just values -> values
      known = [(n,v) | n <- requested, Just v <- [lookup n properties]]
      missing = [n | n <- requested, lookup n properties == Nothing]
      propstat code values = if null values then "" else "<D:propstat><D:prop>" <> T.concat values <> "</D:prop><D:status>HTTP/1.1 " <> code <> "</D:status></D:propstat>"
  in "<D:response>" <> hrefXml href
    <> propstat "200 OK" [tag n (if namesOnly then "" else v) | (n,v) <- known]
    <> propstat "404 Not Found" [tag n "" | n <- missing]
    <> "</D:response>"

missingResponse :: Text -> Text
missingResponse href = "<D:response>" <> hrefXml href <> "<D:status>HTTP/1.1 404 Not Found</D:status></D:response>"

objectProperties :: DavObject -> [(Name,Text)]
objectProperties o = [(X.name X.dav "getetag", X.xmlEscape (davObjectEtag o)), (X.name X.dav "getcontenttype", X.xmlEscape (davObjectContentType o)), (X.name X.dav "getcontentlength", T.pack (show (LBS.length (davObjectBody o)))), (X.name X.dav "resourcetype", "")]

propertySelection :: Maybe Element -> Maybe [Name]
propertySelection Nothing = Nothing
propertySelection (Just root) = case filter (X.named X.dav "prop") (X.children root) of
  p:_ -> Just (map elementName (X.children p))
  [] -> case filter (X.named X.dav "include") (X.children root) of
    included:_ -> Just (X.name X.dav "allprop" : map elementName (X.children included))
    [] -> Nothing

principalHref :: Maybe Text -> Text
principalHref user = "/dav/principals/" <> Digest.sha256 (TE.encodeUtf8 (fromMaybe "development" user)) <> "/"

commonProperties :: Maybe Text -> [(Name,Text)]
commonProperties user =
  [(X.name X.dav "current-user-principal", hrefXml (principalHref user)), (X.name X.card "addressbook-home-set", hrefXml "/dav/"), (X.name X.cal "calendar-home-set", hrefXml "/dav/calendars/")]

collectionProperties :: Maybe Text -> Collection -> [(Name,Text)]
collectionProperties user c = commonProperties user ++
  [(X.name X.dav "displayname", X.xmlEscape (collectionName c)),
   (X.name X.dav "resourcetype", "<D:collection/>" <> if collectionKind c == "addressbook" then "<C:addressbook/>" else "<CAL:calendar/>"),
   (X.name "http://calendarserver.org/ns/" "getctag", X.xmlEscape (collectionToken c)),
   (X.name X.dav "sync-token", X.xmlEscape (collectionToken c)),
   (X.name X.dav "current-user-privilege-set", "<D:privilege><D:read/></D:privilege>" <> if collectionAccess c == "write" then "<D:privilege><D:write/></D:privilege><D:privilege><D:write-content/></D:privilege><D:privilege><D:bind/></D:privilege><D:privilege><D:unbind/></D:privilege>" else ""),
   (X.name X.dav "supported-report-set", T.concat ["<D:supported-report><D:report><" <> r <> "/></D:report></D:supported-report>" | r <- if collectionKind c == "addressbook" then ["C:addressbook-query","C:addressbook-multiget","D:sync-collection"] else ["CAL:calendar-query","CAL:calendar-multiget","D:sync-collection"]])]
   ++ if collectionKind c == "addressbook" then
     [(X.name X.card "supported-address-data", "<C:address-data-type content-type=\"text/vcard\" version=\"3.0\"/><C:address-data-type content-type=\"text/vcard\" version=\"4.0\"/>"), (X.name X.card "max-resource-size", "10485760")]
   else [(X.name X.cal "calendar-timezone", X.xmlEscape (fromMaybe "" (Native.timezoneCalendar (collectionTimezone c)))), (X.name X.cal "supported-calendar-component-set", "<CAL:comp name=\"VEVENT\"/><CAL:comp name=\"VTODO\"/>"), (X.name X.cal "supported-calendar-data", "<CAL:calendar-data content-type=\"text/calendar\" version=\"2.0\"/>"), (X.name X.cal "max-resource-size", "10485760")]

collectionObjects :: Store -> Collection -> IO [DavObject]
collectionObjects store c = if collectionKind c == "addressbook" then storeListContactObjects store else storeListCalendarObjectsInCollection store (T.dropEnd 1 (T.drop (T.length "/dav/calendars/") (collectionHref c)))

propfind :: Maybe Text -> Store -> ([DavObject] -> IO [DavObject]) -> [Text] -> Maybe Text -> LBS.ByteString -> IO Response
propfind user store present segments depth body
  | depth `notElem` [Just "0", Just "1"] = pure (xmlResponse 403 ("<D:error" <> namespaces <> "><D:propfind-finite-depth/></D:error>"))
  | otherwise = case if LBS.null body then Right Nothing else Just <$> X.parseXml body of
    Left message -> pure (xmlResponse 400 ("<D:error" <> namespaces <> "><D:responsedescription>" <> X.xmlEscape message <> "</D:responsedescription></D:error>"))
    Right root | maybe False (not . X.named X.dav "propfind") root -> pure (xmlResponse 400 "<error>Expected DAV:propfind</error>")
    Right root -> do
      available <- storeCollections store user
      let href = "/dav/" <> T.intercalate "/" segments <> if null segments then "" else "/"
          selected = propertySelection root
          namesOnly = maybe False (any (X.named X.dav "propname") . X.children) root
          response h ps = propResponse h ps selected namesOnly
          homeProperties = commonProperties user ++ [(X.name X.dav "displayname", "mates"), (X.name X.dav "resourcetype", "<D:collection/>")]
          principal = principalHref user
      case segments of
        [] -> pure (multistatus (response "/dav/" homeProperties : if depth == Just "1" then response "/dav/calendars/" homeProperties : [response (collectionHref c) (collectionProperties user c) | c <- available, collectionKind c == "addressbook"] else []) "")
        ["calendars"] -> pure (multistatus (response "/dav/calendars/" homeProperties : if depth == Just "1" then [response (collectionHref c) (collectionProperties user c) | c <- available, collectionKind c == "calendar"] else []) "")
        ["principals", _] | href == principal -> pure (multistatus [response principal (commonProperties user ++ [(X.name X.dav "resourcetype", "<D:principal/>"), (X.name X.dav "displayname", X.xmlEscape (fromMaybe "development" user))])] "")
        _ -> case filter ((== href) . collectionHref) available of
          [c] -> do
            objects <- if depth == Just "1" then present =<< collectionObjects store c else pure []
            pure (multistatus (response href (collectionProperties user c) : [response (davObjectHref o) (objectProperties o) | o <- objects]) "")
          _ -> do
            let objectHref = T.dropEnd 1 href
            contact <- storeLookupContactObject store objectHref
            calendar <- storeLookupCalendarObject store objectHref
            objects <- present ([o | Just o <- [contact,calendar]])
            pure (if null objects then xmlResponse 404 "<error>Resource not found</error>" else multistatus [response (davObjectHref o) (objectProperties o) | o <- objects] "")

report :: Text -> Store -> ([DavObject] -> IO [DavObject]) -> [Text] -> LBS.ByteString -> IO Response
report origin store present segments body = case X.parseXml body of
  Left message -> pure (xmlResponse 400 ("<error>" <> X.xmlEscape message <> "</error>"))
  Right root -> do
    available <- storeCollections store Nothing
    let href = "/dav/" <> T.intercalate "/" segments <> "/"
    case filter ((== href) . collectionHref) available of
      [collection] -> process collection root
      _ -> pure (xmlResponse 403 "<error>Reports require an accessible collection</error>")
  where
    process c root
      | X.named X.dav "sync-collection" root = do
          let previous = X.contents <$> listToMaybe (filter (X.named X.dav "sync-token") (X.children root))
              limit = listToMaybe [n | e <- X.descendants root, X.named X.dav "nresults" e, [(n,"")] <- [reads (T.unpack (X.contents e))]]
              level = [X.contents e | e <- X.children root, X.named X.dav "sync-level" e]
          if level /= ["1"] then pure (xmlResponse 403 ("<D:error" <> namespaces <> "><D:sync-traversal-supported/></D:error>")) else do
            result <- storeSyncCollection store (collectionHref c) previous limit
            case result of
              Left _ -> pure (xmlResponse 403 ("<D:error" <> namespaces <> "><D:valid-sync-token/></D:error>"))
              Right sync -> do
                responses <- mapM (syncResponse root) (syncChanges sync)
                pure (multistatus (responses ++ if syncTruncated sync then ["<D:response>" <> hrefXml (collectionHref c) <> "<D:status>HTTP/1.1 507 Insufficient Storage</D:status></D:response>"] else []) ("<D:sync-token>" <> X.xmlEscape (syncToken sync) <> "</D:sync-token>"))
      | validReport c root = do
          raw <- if isMultiget root then mapM (\h -> if collectionHref c `T.isPrefixOf` h && not ("/" `T.isInfixOf` T.drop (T.length (collectionHref c)) h) then lookupObject h else pure []) (requestedHrefs c root) >>= pure . concat else case sqlCandidate root of Just q | collectionKind c == "addressbook" -> storeSearchContactObjects store (Just q); _ -> collectionObjects store c
          let predicates = [e | e <- X.children root, X.named X.card "filter" e || X.named X.cal "filter" e]
          case traverse validateFilter predicates >> validateData root of
            Left problem -> pure (xmlResponse 403 ("<D:error" <> namespaces <> "><" <> (if collectionKind c == "addressbook" then "C" else "CAL") <> ":supported-filter/><D:responsedescription>" <> X.xmlEscape problem <> "</D:responsedescription></D:error>"))
            Right _ -> do
              let matching = filter (\o -> all (matchesFilter c root o) predicates) raw
              objects <- present matching
              let found o = reportObject root o
                  responses = if isMultiget root then [maybe (missingResponse h) found (listToMaybe (filter ((== h) . davObjectHref) objects)) | h <- requestedHrefs c root] else map found objects
              pure (multistatus responses "")
      | otherwise = pure (xmlResponse 403 ("<D:error" <> namespaces <> "><D:supported-report/></D:error>"))
    validReport c root = if collectionKind c == "addressbook" then any (\n -> X.named X.card n root) ["addressbook-query", "addressbook-multiget"] else any (\n -> X.named X.cal n root) ["calendar-query", "calendar-multiget"]
    isMultiget root = nameLocalName (elementName root) `elem` ["addressbook-multiget","calendar-multiget"]
    requestedHrefs _ root = [normalizeHref (X.contents e) | e <- X.children root, X.named X.dav "href" e]
    normalizeHref h =
      let trimmed = T.strip h
          absolute = "http://" `T.isPrefixOf` trimmed || "https://" `T.isPrefixOf` trimmed
          path = if absolute && (origin <> "/") `T.isPrefixOf` trimmed then T.drop (T.length origin) trimmed else trimmed
      in if absolute && path == trimmed then trimmed else TE.decodeUtf8With lenientDecode (urlDecode False (TE.encodeUtf8 path))

    lookupObject h = do
      contact <- storeLookupContactObject store h
      calendar <- storeLookupCalendarObject store h
      pure [o | Just o <- [contact,calendar], davObjectHref o == h]
    syncResponse root (h,deleted) = if deleted then pure (missingResponse h) else do
      objects <- present =<< lookupObject h
      pure (maybe (missingResponse h) (reportObject root) (listToMaybe objects))

reportObject :: Element -> DavObject -> Text
reportObject root o =
  let selected = propertySelection (Just root)
      dataRequests = [e | p <- X.children root, X.named X.dav "prop" p, e <- X.children p, X.named X.card "address-data" e || X.named X.cal "calendar-data" e]
      body = TE.decodeUtf8With lenientDecode (LBS.toStrict (davObjectBody o))
      withData = objectProperties o ++ [(elementName e, X.xmlEscape (selectData e body)) | e <- dataRequests]
  in propResponse (davObjectHref o) withData selected False

selectData :: Element -> Text -> Text
selectData selection body
  | X.named X.card "address-data" selection =
      let names = [T.toUpper n | e <- X.children selection, X.named X.card "prop" e, Just n <- [X.attribute "name" e]]
      in if null names then body else T.intercalate "\r\n" [if any (\e -> X.attribute "name" e == Just property && X.attribute "novalue" e == Just "yes") (X.children selection) then fst (T.breakOn ":" line) <> ":" else line | raw <- unfoldLines (lines (T.unpack body)), let line = T.dropWhileEnd (== '\r') (T.pack raw), let property = T.toUpper (last (T.splitOn "." (T.takeWhile (\c -> c /= ';' && c /= ':') line))), property `elem` (["BEGIN","END","VERSION"] ++ names)] <> "\r\n"
  | X.named X.cal "calendar-data" selection =
      case filter (X.named X.cal "comp") (X.children selection) of
        [] -> body
        requested -> T.concat [serializeCalendar requestedNode component | requestedNode <- requested, component <- IC.parseCalendarTree body, X.attribute "name" requestedNode == Just (IC.componentKind component)]
  | otherwise = body

serializeCalendar :: Element -> IC.ICalComponent -> Text
serializeCalendar selection component =
  let requested = [T.toUpper key | child <- X.children selection, X.named X.cal "prop" child, Just key <- [X.attribute "name" child]]
      allProperties = any (X.named X.cal "allprop") (X.children selection)
      allComponents = any (X.named X.cal "allcomp") (X.children selection)
      selectedComponents = filter (X.named X.cal "comp") (X.children selection)
      linesFor p = IC.propertyName p <> T.concat [";" <> k <> "=" <> (if T.any (`elem` [';', ':', ',']) v then "\"" <> v <> "\"" else v) | (k,v) <- IC.propertyParameters p] <> ":" <> IC.propertyValue p
      full c = "BEGIN:" <> IC.componentKind c <> "\r\n" <> T.concat [linesFor p <> "\r\n" | p <- IC.componentProperties c] <> T.concat (map full (IC.componentChildren c)) <> "END:" <> IC.componentKind c <> "\r\n"
  in if null (X.children selection) then full component else
     "BEGIN:" <> IC.componentKind component <> "\r\n"
       <> T.concat [(if any (\e -> X.attribute "name" e == Just (IC.propertyName p) && X.attribute "novalue" e == Just "yes") (X.children selection) then fst (T.breakOn ":" (linesFor p)) <> ":" else linesFor p) <> "\r\n" | p <- IC.componentProperties component, allProperties || IC.propertyName p `elem` requested]
       <> (if allComponents then T.concat (map full (IC.componentChildren component)) else T.concat [serializeCalendar childSelection child | childSelection <- selectedComponents, child <- IC.componentChildren component, X.attribute "name" childSelection == Just (IC.componentKind child)])
       <> "END:" <> IC.componentKind component <> "\r\n"

validateFilter :: Element -> Either Text ()
validateFilter e
  | nameNamespace n `notElem` [Just X.card, Just X.cal] = Left "Unknown filter namespace"
  | nameLocalName n `notElem` ["filter","prop-filter","param-filter","text-match","is-not-defined","comp-filter","time-range"] = Left "Unsupported filter element"
  | maybe False (`notElem` ["anyof","allof"]) (X.attribute "test" e) = Left "Unsupported filter test"
  | maybe False (`notElem` ["contains","equals","starts-with","ends-with"]) (X.attribute "match-type" e) = Left "Unsupported match type"
  | maybe False (`notElem` ["i;unicode-casemap","i;ascii-casemap","i;octet"]) (X.attribute "collation" e) = Left "Unsupported collation"
  | X.named X.cal "comp-filter" e && maybe True (`notElem` ["VCALENDAR","VEVENT","VTODO"]) (X.attribute "name" e) = Left "Unsupported calendar component filter"
  | nameLocalName n `elem` ["prop-filter", "param-filter"] && maybe True T.null (X.attribute "name" e) = Left "Filter name is required"
  | X.named X.cal "time-range" e && not (validTimeRange e) = Left "Invalid calendar time range"
  | otherwise = mapM_ validateFilter (X.children e)
  where n = elementName e

matchesFilter :: Collection -> Element -> DavObject -> Element -> Bool
matchesFilter collection queryRoot object root
  | "text/vcard" `T.isPrefixOf` davObjectContentType object = case parseVCards (T.unpack text) of
      Right [card] -> matchProperties (parseProperties card) root
      _ -> False
  | otherwise = matchCalendar (floatingContext collection queryRoot text) root
  where text = TE.decodeUtf8With lenientDecode (LBS.toStrict (davObjectBody object))

combine :: Element -> [Bool] -> Bool
combine _ [] = True
combine e values = if X.attribute "test" e == Just "allof" then and values else or values

matchProperties :: [VCardProperty] -> Element -> Bool
matchProperties properties e = case nameLocalName (elementName e) of
  "filter" -> combine e (map (matchProperties properties) (X.children e))
  "prop-filter" ->
    let key = T.toUpper (fromMaybe "" (X.attribute "name" e))
        qualified p = maybe "" ((<> ".") . T.pack) (propertyGroup p) <> T.pack (propertyName p)
        found = filter (\p -> key == T.toUpper (if "." `T.isInfixOf` key then qualified p else T.pack (propertyName p))) properties
    in if any (X.named X.card "is-not-defined") (X.children e) then null found else not (null found) && any (\p -> combine e [matchProperty child p | child <- X.children e]) found
  _ -> False
  where
    matchProperty child p = case nameLocalName (elementName child) of
      "text-match" -> matchText child (T.pack (decodeText (propertyRawValue p)))
      "param-filter" ->
        let values = [value | (k,v) <- propertyParams p, T.toUpper (T.pack k) == T.toUpper (fromMaybe "" (X.attribute "name" child)), let unquoted = T.dropAround (== '"') (T.pack v), value <- if k == "TYPE" then T.splitOn "," unquoted else [unquoted]]
        in if any (X.named X.card "is-not-defined") (X.children child) then null values else not (null values) && all (\predicate -> any (matchText predicate) values) (X.children child)
      _ -> False

matchText :: Element -> Text -> Bool
matchText e raw =
  let normalize = case X.attribute "collation" e of Just "i;octet" -> id; Just "i;ascii-casemap" -> asciiFold; _ -> if nameNamespace (elementName e) == Just X.cal then asciiFold else Normalize.normalize Normalize.NFKD . T.map toTitle
      asciiFold = T.map (\c -> if c >= 'A' && c <= 'Z' then toLower c else c)
      needle = normalize (X.contents e); haystack = normalize raw
      found = case X.attribute "match-type" e of Just "equals" -> needle == haystack; Just "starts-with" -> needle `T.isPrefixOf` haystack; Just "ends-with" -> needle `T.isSuffixOf` haystack; _ -> needle `T.isInfixOf` haystack
  in if X.attribute "negate-condition" e == Just "yes" then not found else found

matchCalendar :: Text -> Element -> Bool
matchCalendar body root = any (\component -> match component root) (IC.parseCalendarTree (either (const body) id (Native.normalizeCalendar body)))
  where
    match component e = case nameLocalName (elementName e) of
      "filter" -> all (match component) (X.children e)
      "comp-filter" ->
        let kind = fromMaybe "VCALENDAR" (X.attribute "name" e)
            candidates = if IC.componentKind component == kind then [component] else filter ((== kind) . IC.componentKind) (IC.componentChildren component)
            predicates = filter (not . X.named X.cal "is-not-defined") (X.children e)
        in if any (X.named X.cal "is-not-defined") (X.children e) then null candidates else any (\candidate -> all (match candidate) predicates) candidates
      "time-range" -> IC.calendarObjectMatchesTimeRange (X.attribute "start" e) (X.attribute "end" e) body
      "prop-filter" ->
        let key = T.toUpper (fromMaybe "" (X.attribute "name" e))
            values = filter ((== key) . IC.propertyName) (IC.componentProperties component)
        in if any (X.named X.cal "is-not-defined") (X.children e) then null values else not (null values) && all (\predicate -> any (matchProperty predicate) values) (X.children e)
      _ -> False
    matchProperty e p = case nameLocalName (elementName e) of
      "text-match" -> matchText e (IC.propertyValue p)
      "param-filter" ->
        let values = [value | (key,value) <- IC.propertyParameters p, key == T.toUpper (fromMaybe "" (X.attribute "name" e))]
        in if any (X.named X.cal "is-not-defined") (X.children e) then null values else not (null values) && all (\predicate -> any (matchText predicate) values) (X.children e)
      "time-range" ->
        let parse value = parseTimeM True defaultTimeLocale "%Y%m%dT%H%M%SZ" (T.unpack value) :: Maybe UTCTime
        in case parse (IC.propertyValue p) of
          Nothing -> False
          Just value -> maybe True (<= value) (X.attribute "start" e >>= parse) && maybe True (> value) (X.attribute "end" e >>= parse)
      _ -> False

validTimeRange :: Element -> Bool
validTimeRange e =
  let parse value = parseTimeM True defaultTimeLocale "%Y%m%dT%H%M%SZ" (T.unpack value) :: Maybe UTCTime
      rawStart = X.attribute "start" e
      rawEnd = X.attribute "end" e
      start = rawStart >>= parse
      end = rawEnd >>= parse
   in (rawStart /= Nothing || rawEnd /= Nothing) && (rawStart == Nothing || start /= Nothing) && (rawEnd == Nothing || end /= Nothing) && case (start,end) of (Just a,Just b) -> a < b; _ -> True

validateData :: Element -> Either Text ()
validateData root = mapM_ validate [dataElement | prop <- X.children root, X.named X.dav "prop" prop, dataElement <- X.children prop, X.named X.card "address-data" dataElement || X.named X.cal "calendar-data" dataElement]
  where
    validate e
      | nameNamespace (elementName e) `notElem` [Just X.card, Just X.cal] = Left "Unknown data selection namespace"
      | nameLocalName (elementName e) `notElem` ["address-data","calendar-data","comp","prop","allprop","allcomp"] = Left "Unsupported data selection"
      | nameLocalName (elementName e) `elem` ["comp","prop"] && maybe True T.null (X.attribute "name" e) = Left "Data selection requires a name"
      | maybe False (`notElem` ["yes","no"]) (X.attribute "novalue" e) = Left "Invalid novalue attribute"
      | otherwise = mapM_ validate (X.children e)

-- Push only a necessary, SQL-compatible predicate. The complete XML filter
-- still runs over candidates, including arbitrary extension properties.
sqlCandidate :: Element -> Maybe Text
sqlCandidate root = listToMaybe
  [X.contents textMatch | f <- X.children root, X.named X.card "filter" f,
   X.attribute "test" f == Just "allof" || length (X.children f) == 1,
   p <- X.children f, X.named X.card "prop-filter" p,
   X.attribute "name" p `elem` map Just ["FN","UID","EMAIL"],
   X.attribute "test" p == Just "allof" || length (X.children p) == 1,
   textMatch <- X.children p, X.named X.card "text-match" textMatch,
   X.attribute "collation" textMatch == Just "i;ascii-casemap",
   X.attribute "negate-condition" textMatch /= Just "yes",
   X.attribute "match-type" textMatch `elem` [Nothing,Just "contains"]]

-- Floating values are interpreted in the query timezone, or the collection's
-- timezone when the query supplies none. This is a query view, never a rewrite
-- of the authoritative object or its floating-time projection.
floatingContext :: Collection -> Element -> Text -> Text
floatingContext collection queryRoot body =
  let requested = listToMaybe [X.contents e | e <- X.children queryRoot, X.named X.cal "timezone" e]
      definitions source = [node | root <- IC.parseCalendarTree source, node <- IC.componentChildren root, IC.componentKind node == "VTIMEZONE"]
      timezoneId source = listToMaybe [IC.propertyValue p | node <- definitions source, p <- IC.componentProperties node, IC.propertyName p == "TZID"]
      zone = if requested == Nothing then collectionTimezone collection else "urn:mates:query-timezone:" <> Digest.sha256 (TE.encodeUtf8 (fromMaybe "" requested))
      definition = maybe "" (T.concat . map serializeTimezone . definitions) requested
      assign line =
        let (left,colonValue) = T.breakOn ":" line
            property = T.toUpper (T.takeWhile (/= ';') left)
            value = T.drop 1 colonValue
            firstValue = T.takeWhile (\c -> c /= ',' && c /= '/') value
        in if property `elem` ["DTSTART","DTEND","DUE","RECURRENCE-ID","RDATE","EXDATE"] && not ("TZID=" `T.isInfixOf` T.toUpper left) && T.length firstValue > 8 && not ("Z" `T.isSuffixOf` firstValue)
           then left <> ";TZID=" <> zone <> colonValue else line
      rewrite _ [] = []
      rewrite inZone (line:rest)
        | T.toUpper line == "BEGIN:VTIMEZONE" = line : rewrite True rest
        | T.toUpper line == "END:VTIMEZONE" = line : rewrite False rest
        | otherwise = (if inZone then line else assign line) : rewrite inZone rest
      rewritten = T.intercalate "\r\n" (rewrite False (map (T.dropWhileEnd (== '\r') . T.pack) (unfoldLines (lines (T.unpack body)))))
      result = if T.null definition then rewritten else T.replace "END:VCALENDAR" (definition <> "END:VCALENDAR") rewritten
  in if requested /= Nothing && (requested >>= timezoneId) == Nothing then throw (Native.CalendarFailure "Invalid query timezone") else
       case Native.normalizeCalendar result of Left message -> throw (Native.CalendarFailure (T.unpack message)); Right _ -> result
  where
    serializeTimezone component = "BEGIN:" <> IC.componentKind component <> "\r\n" <> T.concat [IC.propertyName p <> T.concat [";" <> k <> "=" <> v | (k,v) <- IC.propertyParameters p] <> ":" <> (if IC.propertyName p == "TZID" then "urn:mates:query-timezone:" <> Digest.sha256 (TE.encodeUtf8 (fromMaybe "" (listToMaybe [X.contents e | e <- X.children queryRoot, X.named X.cal "timezone" e]))) else IC.propertyValue p) <> "\r\n" | p <- IC.componentProperties component] <> T.concat (map serializeTimezone (IC.componentChildren component)) <> "END:" <> IC.componentKind component <> "\r\n"
