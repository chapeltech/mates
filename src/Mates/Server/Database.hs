{-# LANGUAGE OverloadedStrings #-}
module Mates.Server.Database
  ( Collection (..), SyncResult (..), collections, syncCollection, admin, principalEnabled, lockCollections, backfillChanges ) where

import Control.Monad (forM_, void)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Text.Read (readMaybe)
import qualified Mates.CalendarNative as Native

data Collection = Collection
  { collectionId :: Text, collectionHref :: Text, collectionName :: Text
  , collectionTimezone :: Text
  , collectionKind :: Text, collectionAccess :: Text, collectionToken :: Text
  } deriving (Eq, Show)

data SyncResult = SyncResult
  { syncToken :: Text, syncChanges :: [(Text, Bool)], syncTruncated :: Bool
  } deriving (Eq, Show)

token :: Text -> Text -> Int64 -> Text
token ident generation sequenceNo = T.intercalate ":" ["urn:mates:sync", ident, generation, T.pack (show sequenceNo)]

collections :: Connection -> Maybe Text -> IO [Collection]
collections conn principal = do
  rows <- query conn
    "SELECT c.id::text, c.url, coalesce(c.display_name, c.url), c.collection_kind, CASE WHEN ?::text IS NULL THEN 'write' ELSE g.access END, c.sync_generation::text, c.change_sequence, coalesce(cal.timezone, 'UTC') FROM dav_collections c LEFT JOIN calendars cal ON cal.id = c.calendar_id LEFT JOIN collection_grants g ON g.collection_id = c.id AND g.username = ? LEFT JOIN local_principals p ON p.username = g.username WHERE c.account_id IN (SELECT id FROM dav_accounts WHERE server_url = 'local') AND (?::text IS NULL OR (p.enabled AND g.access IS NOT NULL)) ORDER BY c.url"
    (principal, principal, principal) :: IO [(Text, Text, Text, Text, Text, Text, Int64, Text)]
  pure [Collection i h n zone k a (token i g s) | (i,h,n,k,a,g,s,zone) <- rows]

principalEnabled :: Connection -> Text -> IO Bool
principalEnabled conn username = do
  rows <- query conn "SELECT enabled FROM local_principals WHERE username = ?" (Only username)
  pure (rows == [Only True])

-- Serialize object writes, including derived inverse relationships, in a
-- deterministic collection order. Recheck permissions under these locks.
lockCollections :: Connection -> IO ()
lockCollections conn = void (query_ conn "SELECT id::text FROM dav_collections ORDER BY id FOR UPDATE" :: IO [Only Text])

backfillChanges :: Connection -> IO ()
backfillChanges conn = do
  rows <- query_ conn "SELECT o.id::text, o.deleted_at IS NOT NULL FROM dav_objects o WHERE NOT EXISTS (SELECT 1 FROM dav_collection_changes ch WHERE ch.collection_id = o.collection_id AND ch.href = o.href) ORDER BY o.collection_id, o.href" :: IO [(Text, Bool)]
  forM_ rows $ \(i,d) -> void (query conn "SELECT mates_record_change(?::uuid, ?)" (i,d) :: IO [Only ()])

syncCollection :: Connection -> Text -> Maybe Text -> Maybe Int -> IO (Either Text SyncResult)
syncCollection conn href previous requestedLimit = do
  rows <- query conn "SELECT id::text, sync_generation::text, change_sequence FROM dav_collections WHERE url = ?" (Only href) :: IO [(Text,Text,Int64)]
  case rows of
    [(ident, generation, current)] -> do
      let prefix = token ident generation 0
          parsePrevious value = case T.stripPrefix (T.dropEnd 1 prefix) value of
            Just suffix -> readMaybe (T.unpack suffix) :: Maybe Int64
            Nothing -> Nothing
          initial = maybe True T.null previous
          since = if initial then Just 0 else previous >>= parsePrevious
      case since of
        Just n | n >= 0 && n <= current -> do
          let limit = max 1 (min 1000 (maybe 1000 id requestedLimit))
          changes <- query conn
            "SELECT href, deleted, sequence FROM (SELECT DISTINCT ON (href) href, deleted, sequence FROM dav_collection_changes WHERE collection_id = ?::uuid AND sequence > ? AND sequence <= ? ORDER BY href, sequence DESC) latest WHERE NOT (? AND deleted) ORDER BY sequence LIMIT ?"
            (ident, n, current, initial, limit + 1) :: IO [(Text,Bool,Int64)]
          let more = length changes > limit
              selected = take limit changes
              lastSequence = if more then maximum (n : [s | (_,_,s) <- selected]) else current
          pure (Right (SyncResult (token ident generation lastSequence) [(h,d) | (h,d,_) <- selected] more))
        _ -> pure (Left "valid-sync-token")
    _ -> pure (Left "collection-not-found")

admin :: Connection -> [Text] -> IO (Either Text [Text])
admin conn args = case args of
  ["principal", "add", username] | not (T.null (T.strip username)) && not (T.any (`elem` ['\r','\n','\0',',']) username) -> do
    void $ execute conn "INSERT INTO local_principals(username) VALUES (?) ON CONFLICT (username) DO UPDATE SET enabled = true" (Only username)
    pure (Right ["Enabled " <> username])
  ["principal", "disable", username] -> do
    void $ execute conn "UPDATE local_principals SET enabled = false WHERE username = ?" (Only username)
    pure (Right ["Disabled " <> username])
  ["principal", "list"] -> do
    rows <- query_ conn "SELECT username, enabled FROM local_principals ORDER BY username" :: IO [(Text,Bool)]
    pure (Right [u <> if e then "\tenabled" else "\tdisabled" | (u,e) <- rows])
  ["collection", "list"] -> Right . map (\c -> collectionHref c <> "\t" <> collectionName c) <$> collections conn Nothing
  ["collection", "create", href, name] | validCalendar href -> do
    let calendarName = T.dropEnd 1 (T.drop (T.length "/dav/calendars/") href)
    void $ execute conn "INSERT INTO calendars(display_name, timezone) SELECT ?, 'UTC' WHERE NOT EXISTS (SELECT 1 FROM dav_collections WHERE url = ?)" (name,href)
    void $ execute conn "INSERT INTO dav_collections(account_id, calendar_id, collection_kind, url, display_name) SELECT a.id, c.id, 'calendar', ?, ? FROM dav_accounts a CROSS JOIN LATERAL (SELECT id FROM calendars WHERE display_name = ? ORDER BY created_at DESC LIMIT 1) c WHERE a.server_url = 'local' AND a.username = 'mates' ON CONFLICT (account_id, url) DO NOTHING" (href,name,name)
    pure (Right ["Collection " <> calendarName <> " ready"])
  ["collection", "timezone", href, zone] | Native.timezoneCalendar zone /= Nothing -> do
    count <- execute conn "UPDATE calendars SET timezone = ? WHERE id IN (SELECT calendar_id FROM dav_collections WHERE url = ?)" (zone,href)
    void $ execute conn "UPDATE dav_collections SET change_sequence = change_sequence + 1 WHERE url = ? AND calendar_id IS NOT NULL" (Only href)
    pure (if count == 0 then Left "Unknown calendar collection" else Right ["Updated calendar timezone"])
  ["access", "list"] -> do
    rows <- query_ conn "SELECT g.username, c.url, g.access FROM collection_grants g JOIN dav_collections c ON c.id = g.collection_id ORDER BY g.username, c.url" :: IO [(Text,Text,Text)]
    pure (Right [T.intercalate "\t" [u,h,a] | (u,h,a) <- rows])
  ["access", "grant", username, href, access] | access `elem` ["read", "write"] -> do
    count <- execute conn "INSERT INTO collection_grants(username, collection_id, access) SELECT p.username, c.id, ? FROM local_principals p CROSS JOIN dav_collections c WHERE p.username = ? AND c.url = ? ON CONFLICT (username, collection_id) DO UPDATE SET access = EXCLUDED.access" (access,username,href)
    pure (if count == 0 then Left "Unknown principal or collection" else Right ["Granted " <> access])
  ["access", "revoke", username, href] -> do
    void $ execute conn "DELETE FROM collection_grants g USING dav_collections c WHERE g.collection_id = c.id AND g.username = ? AND c.url = ?" (username,href)
    pure (Right ["Revoked access"])
  _ -> pure (Left "Usage: principal add|disable USER; principal list; collection list; collection create /dav/calendars/NAME/ DISPLAY_NAME; collection timezone COLLECTION IANA_ZONE; access list; access grant USER COLLECTION read|write; access revoke USER COLLECTION")
  where
    validCalendar href = case T.splitOn "/" href of
      ["", "dav", "calendars", name, ""] -> not (T.null name) && name /= "." && name /= ".." && not (T.any (`elem` ['%', '?', '#', '\\']) name)
      _ -> False
