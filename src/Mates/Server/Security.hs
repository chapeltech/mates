{-# LANGUAGE OverloadedStrings #-}
module Mates.Server.Security
  ( AuthMode (..), SecurityConfig (..), developmentSecurity, validPublicUrl, authenticate, scopedStore, mayAccess ) where

import Data.Text (Text)
import Network.URI (URI (..), URIAuth (..), parseURI)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Network.Wai (Request, requestHeaders)
import Mates.Server.Store (Store (..))
import Mates.Server.Types (DavObject (..), CalendarSummary (..), CalendarEntrySummary (..), TodoSummary (..))
import Mates.Server.Database (Collection (..))

data AuthMode = ProxyAuth | NoAuth deriving (Eq, Show)
data SecurityConfig = SecurityConfig { authMode :: AuthMode, publicUrl :: Text } deriving (Eq, Show)

developmentSecurity :: SecurityConfig
developmentSecurity = SecurityConfig NoAuth "http://127.0.0.1:8080"

authenticate :: SecurityConfig -> Store -> Request -> IO (Either Int (Maybe Text))
authenticate config store request = case authMode config of
  NoAuth -> pure (Right Nothing)
  ProxyAuth -> case [v | (k,v) <- requestHeaders request, k == "X-REMOTE-USER"] of
    [value] | not (BS.null value) -> case TE.decodeUtf8' value of
      Right username | not (T.null (T.strip username)) && not (T.any (`elem` ['\r','\n', '\0', ',']) username) -> do
        enabled <- storePrincipalEnabled store username
        pure (if enabled then Right (Just username) else Left 403)
      _ -> pure (Left 401)
    _ -> pure (Left 401)

mayAccess :: [Collection] -> Bool -> Text -> Bool
mayAccess allowed writing href = any matches allowed
  where matches c = collectionHref c `T.isPrefixOf` href && (not writing || collectionAccess c == "write")

-- Every lookup is scoped, including multiget hrefs supplied in an XML body.
-- The HTTP boundary handles denial; the store also prevents accidental leakage.
scopedStore :: [Collection] -> Store -> Store
scopedStore allowed store = store
  { storeCollections = const (pure allowed)
  , storeListContacts = \q -> if readable "/dav/contacts/" then storeListContacts store q else pure []
  , storeCreateContact = \contact -> if writable "/dav/contacts/" then storeCreateContact store contact else ioError (userError "Forbidden")
  , storeListCalendars = filter (\c -> calendarSummaryDisplayName c `elem` calendarNames) <$> storeListCalendars store
  , storeListCalendarEntries = \q -> filter (\entry -> calendarEntryCalendarId entry `elem` calendarNames) <$> storeListCalendarEntries store q
  , storeListTodos = \q -> filter (\todo -> todoSummaryCalendarId todo `elem` calendarNames) <$> storeListTodos store q
  , storeLookupDavObject = \segments -> if readable ("/dav/" <> T.intercalate "/" segments) then storeLookupDavObject store segments else pure Nothing
  , storeSyncCollection = \href previous limit -> if readable href then storeSyncCollection store href previous limit else pure (Left "Forbidden")
  , storeAdmin = const (pure (Left "Operator commands require direct database access"))
  , storeTransaction = \actor userAgent writing action -> storeTransaction store actor userAgent writing (action . scopedStore allowed)
  , storeListContactObjects = visible <$> storeListContactObjects store
  , storeSearchContactObjects = \q -> visible <$> storeSearchContactObjects store q
  , storeLookupContactObject = \h -> if readable h then storeLookupContactObject store h else pure Nothing
  , storeLookupContactObjects = \hs -> visible <$> storeLookupContactObjects store (filter readable hs)
  , storeListCalendarObjects = visible <$> storeListCalendarObjects store
  , storeListCalendarObjectsInCollection = \n -> visible <$> storeListCalendarObjectsInCollection store n
  , storeSearchCalendarObjects = \c a b -> visible <$> storeSearchCalendarObjects store c a b
  , storeLookupCalendarObject = \h -> if readable h then storeLookupCalendarObject store h else pure Nothing
  , storeLookupCalendarObjects = \hs -> visible <$> storeLookupCalendarObjects store (filter readable hs)
  , storePutContactObject = \h b -> if writable h then storePutContactObject store h b else pure (Left "Forbidden")
  , storePutCalendarObject = \h b -> if writable h then storePutCalendarObject store h b else pure (Left "Forbidden")
  , storeDeleteContactObject = \h -> if writable h then storeDeleteContactObject store h else pure False
  , storeDeleteCalendarObject = \h -> if writable h then storeDeleteCalendarObject store h else pure False
  }
  where
    calendarNames = concat [[collectionName c, T.dropEnd 1 (T.drop (T.length "/dav/calendars/") (collectionHref c))] | c <- allowed, collectionKind c == "calendar"]
    readable = mayAccess allowed False
    writable = mayAccess allowed True
    visible = filter (readable . davObjectHref)

validPublicUrl :: Text -> Bool
validPublicUrl value = case parseURI (T.unpack value) of
  Just uri -> uriScheme uri `elem` ["https:","http:"] && uriPath uri `elem` ["","/"] && null (uriQuery uri) && null (uriFragment uri) && case uriAuthority uri of
    Just authority -> not (null (uriRegName authority)) && null (uriUserInfo authority)
    Nothing -> False
  Nothing -> False
