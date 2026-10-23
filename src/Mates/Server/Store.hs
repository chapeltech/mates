{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mates.Server.Store
  ( DavConflict (..)
  , PostgresConfig (..)
  , Store (..)
  , defaultPostgresConfig
  , newPostgresStore
  , newPostgresStoreFromEnvironment
  , newPostgresStoreFromEnvironmentWithOrigin
  , resetPostgresStore
  )
where

import Control.Exception (bracket, Exception, throwIO)
import qualified Mates.Server.Database as DB
import qualified Mates.Digest as Digest
import qualified Mates.CalendarNative as Native
import Network.HTTP.Types.URI (urlDecode)
import Control.Monad (forM_, void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Base64 as Base64
import Data.Int (Int64)
import Data.Maybe (mapMaybe, maybeToList)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (UnicodeException, lenientDecode)
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , close
  , connectPostgreSQL
  , execute
  , execute_
  , query
  , query_
  , withTransaction
  )
import Database.PostgreSQL.Simple.Types (Binary (..), In (..))
import qualified Mates.ICalendar as ICalendar
import Mates.Server.Types
  ( CalendarEntrySummary (..)
  , CalendarSummary (..)
  , ContactCreate (..)
  , ContactEmail (..)
  , ContactField (..)
  , ContactPhoto (..)
  , ContactRecord (..)
  , ContactRelationship (..)
  , ContactSummary
      ( ContactSummary
      , contactSummaryDisplayName
      , contactSummaryEmails
      , contactSummaryFields
      , contactSummaryId
      , contactSummaryPhoto
      )
  , DavObject (..)
  , DavPayload (..)
  , TodoSummary
      ( TodoSummary
      , todoSummaryCalendarId
      , todoSummaryDue
      , todoSummaryId
      , todoSummaryStatus
      , todoSummaryTitle
      )
  )
import Mates.VCard
  ( VCardProperty (..)
  , parseProperties
  , decodeText
  , VCard (..)
  , VCardEmail (..)
  , VCardField (..)
  , VCardPhoto (..)
  , cardEmailAddresses
  , cardFields
  , cardPhoto
  , displayName
  , parseVCards
  , vcardId
  )
import System.Environment (lookupEnv)

data DavConflict = DavConflict Text deriving (Show)
instance Exception DavConflict

data Store = Store
  { storeTransaction :: forall a. Maybe Text -> Text -> Bool -> (Store -> IO a) -> IO a
  , storePublicOrigin :: Text -> IO ()
  , storeCollections :: Maybe Text -> IO [DB.Collection]
  , storePrincipalEnabled :: Text -> IO Bool
  , storeAdmin :: [Text] -> IO (Either Text [Text])
  , storeSyncCollection :: Text -> Maybe Text -> Maybe Int -> IO (Either Text DB.SyncResult)
  , storeListContacts :: Maybe Text -> IO [ContactSummary]
  , storeCreateContact :: ContactCreate -> IO ContactSummary
  , storeListContactObjects :: IO [DavObject]
  , storeSearchContactObjects :: Maybe Text -> IO [DavObject]
  , storeLookupContactObject :: Text -> IO (Maybe DavObject)
  , storeLookupContactObjects :: [Text] -> IO [DavObject]
  , storePutContactObject :: Text -> LazyByteString.ByteString -> IO (Either Text Bool)
  , storeDeleteContactObject :: Text -> IO Bool
  , storeListCalendars :: IO [CalendarSummary]
  , storeListCalendarEntries :: Maybe Text -> IO [CalendarEntrySummary]
  , storeListCalendarObjects :: IO [DavObject]
  , storeListCalendarObjectsInCollection :: Text -> IO [DavObject]
  , storeSearchCalendarObjects :: Maybe Text -> Maybe Text -> Maybe Text -> IO [DavObject]
  , storeLookupCalendarObject :: Text -> IO (Maybe DavObject)
  , storeLookupCalendarObjects :: [Text] -> IO [DavObject]
  , storePutCalendarObject :: Text -> LazyByteString.ByteString -> IO (Either Text Bool)
  , storeDeleteCalendarObject :: Text -> IO Bool
  , storeListTodos :: Maybe Text -> IO [TodoSummary]
  , storeLookupDavObject :: [Text] -> IO (Maybe DavPayload)
  }

newtype PostgresConfig = PostgresConfig
  { postgresConnectionString :: ByteString8.ByteString
  } deriving (Eq, Show)

data Backend = Standalone PostgresConfig | TransactionConnection Connection

defaultPostgresConfig :: PostgresConfig
defaultPostgresConfig =
  PostgresConfig {postgresConnectionString = "dbname=mates"}

newPostgresStoreFromEnvironment :: IO Store
newPostgresStoreFromEnvironment = newPostgresStoreFromEnvironmentWithOrigin ""

newPostgresStoreFromEnvironmentWithOrigin :: Text -> IO Store
newPostgresStoreFromEnvironmentWithOrigin origin = do
  configured <- lookupEnv "MATES_DATABASE_URL"
  let config = Standalone (PostgresConfig { postgresConnectionString = ByteString8.pack (maybe "dbname=mates" id configured) })
  initializePostgresStore config origin
  pure (postgresStore config)

newPostgresStore :: PostgresConfig -> IO Store
newPostgresStore config = do
  initializePostgresStore (Standalone config) ""
  pure (postgresStore (Standalone config))

postgresStore :: Backend -> Store
postgresStore config =
    Store
      { storeTransaction = postgresTransaction config
      , storePublicOrigin = \origin -> withStoreConnection config (\conn -> void (query conn "SELECT set_config('mates.public_url', ?, true)" (Only origin) :: IO [Only Text]))
      , storeCollections = \principal -> withStoreConnection config (\conn -> DB.collections conn principal)
      , storePrincipalEnabled = \principal -> withStoreConnection config (\conn -> DB.principalEnabled conn principal)
      , storeAdmin = \args -> withStoreConnection config (\conn -> withConfigTransaction config conn (DB.lockCollections conn >> DB.admin conn args))
      , storeSyncCollection = \href previous limit -> withStoreConnection config (\conn -> DB.syncCollection conn href previous limit)
      , storeListContacts = postgresListContacts config
      , storeCreateContact = postgresCreateContact config
      , storeListContactObjects = postgresListContactObjects config
      , storeSearchContactObjects = postgresSearchContactObjects config
      , storeLookupContactObject = postgresLookupContactObject config
      , storeLookupContactObjects = postgresLookupContactObjects config
      , storePutContactObject = postgresPutContactObject config
      , storeDeleteContactObject = postgresDeleteContactObject config
      , storeListCalendars = postgresListCalendars config
      , storeListCalendarEntries = postgresListCalendarEntries config
      , storeListCalendarObjects = postgresListCalendarObjects config
      , storeListCalendarObjectsInCollection = postgresListCalendarObjectsInCollection config
      , storeSearchCalendarObjects = postgresSearchCalendarObjects config
      , storeLookupCalendarObject = postgresLookupCalendarObject config
      , storeLookupCalendarObjects = postgresLookupCalendarObjects config
      , storePutCalendarObject = postgresPutCalendarObject config
      , storeDeleteCalendarObject = postgresDeleteCalendarObject config
      , storeListTodos = postgresListTodos config
      , storeLookupDavObject = postgresLookupDavObject config
      }


-- A request reuses one connection and commits before its response is sent.
-- Existing store operations can participate without opening nested transactions.
withConfigTransaction :: Backend -> Connection -> IO a -> IO a
withConfigTransaction (TransactionConnection _) _ action = action
withConfigTransaction _ conn action = withTransaction conn action

postgresTransaction :: Backend -> Maybe Text -> Text -> Bool -> (Store -> IO a) -> IO a
postgresTransaction config actor userAgent writing action =
  withStoreConnection config $ \conn -> withConfigTransaction config conn $ do
    void (query conn "SELECT set_config('mates.principal', ?, true), set_config('mates.user_agent', ?, true)" (maybe "" id actor, userAgent) :: IO [(Text,Text)])
    if writing then DB.lockCollections conn else pure ()
    action (postgresStore (TransactionConnection conn))

withStoreConnection :: Backend -> (Connection -> IO a) -> IO a
withStoreConnection (TransactionConnection conn) = (\action -> action conn)
withStoreConnection (Standalone config) =
  bracket
    acquire
    close
  where
    acquire = do
      conn <- connectPostgreSQL (postgresConnectionString config)
      void $ execute_ conn "SET client_min_messages TO WARNING"
      pure conn

initializePostgresStore :: Backend -> Text -> IO ()
initializePostgresStore config origin =
  withStoreConnection config $ \conn -> withConfigTransaction config conn $ do
    void (query conn "SELECT set_config('mates.public_url', ?, true)" (Only origin) :: IO [Only Text])
    existing <- query_ conn "SELECT to_regclass('public.dav_collections') IS NOT NULL" :: IO [Only Bool]
    if existing == [Only True] then DB.lockCollections conn else pure ()
    applyDatabaseSchema conn
    backfillDavObjectRevisions conn
    void (ensureContactCollection conn)
    refreshStaleContactProjections conn
    refreshStaleCalendarProjections conn
    refreshAllRelationshipResolutions conn
    DB.backfillChanges conn
    void $ execute conn "UPDATE dav_collections SET sync_generation = gen_random_uuid(), presentation_version = ? WHERE presentation_version IS DISTINCT FROM ?" (currentParserVersion, currentParserVersion)

resetPostgresStore :: PostgresConfig -> IO ()
resetPostgresStore = resetBackend . Standalone

resetBackend :: Backend -> IO ()
resetBackend config =
  withStoreConnection config $ \conn -> withConfigTransaction config conn $ do
    [Only databaseName] <- query_ conn "SELECT current_database()" :: IO [Only Text]
    if "_test" `Text.isSuffixOf` databaseName then pure () else ioError (userError "Refusing to reset a database whose name does not end in _test")
    resetPublicSchema conn
    applyDatabaseSchema conn
    backfillDavObjectRevisions conn
    void (ensureContactCollection conn)
    refreshStaleContactProjections conn
    refreshStaleCalendarProjections conn
    refreshAllRelationshipResolutions conn

applyDatabaseSchema :: Connection -> IO ()
applyDatabaseSchema conn = do
  configuredPath <- lookupEnv "MATES_SCHEMA_PATH"
  sql <- ByteString.readFile (maybe "db/schema.sql" id configuredPath)
  let digest = Digest.sha256 sql
  -- Ordinary startup and operator commands do not rerun unchanged DDL.
  void (query_ conn "SELECT pg_advisory_xact_lock(734028119)" :: IO [Only ()])
  exists <- query_ conn "SELECT to_regclass('public.mates_schema_version') IS NOT NULL" :: IO [Only Bool]
  current <- if exists == [Only True] then query_ conn "SELECT digest FROM mates_schema_version WHERE singleton" else pure []
  if current == [Only digest] then pure () else do
    void $ execute_ conn (fromString (Text.unpack (Text.decodeUtf8 sql)))
    void $ execute conn "INSERT INTO mates_schema_version(singleton, digest) VALUES (true, ?) ON CONFLICT (singleton) DO UPDATE SET digest = EXCLUDED.digest" (Only digest)

resetPublicSchema :: Connection -> IO ()
resetPublicSchema conn = do
  void $ execute_ conn "DROP SCHEMA IF EXISTS public CASCADE"
  void $ execute_ conn "CREATE SCHEMA public"

backfillDavObjectRevisions :: Connection -> IO ()
backfillDavObjectRevisions conn = do
  rows <-
    ( query_
        conn
        "SELECT id::text, object_kind, coalesce(etag, ''), resource_uid, raw_payload \
        \FROM dav_objects \
        \WHERE current_revision_id IS NULL \
        \AND raw_payload IS NOT NULL"
        :: IO [(Text, Text, Text, Maybe Text, Text)]
    )
  forM_ rows $ \(objectId, objectKind, legacyEtag, resourceUid, rawPayloadText) -> do
    let rawPayload = Text.encodeUtf8 rawPayloadText
        etag =
          if Text.null legacyEtag then
            etagForBytes rawPayload
          else
            legacyEtag
    revisionId <-
      appendDavObjectRevision
        conn
        objectId
        "import"
        "system"
        (contentTypeForKind objectKind)
        objectKind
        resourceUid
        etag
        (Just rawPayload)
        "ok"
        Nothing
    void $
      execute
        conn
        "UPDATE dav_objects \
        \SET current_revision_id = ?::uuid, updated_at = now(), \
        \deleted_at = NULL \
        \WHERE id = ?::uuid"
        (revisionId, objectId)

refreshStaleContactProjections :: Connection -> IO ()
refreshStaleContactProjections conn = do
  rows <-
    query
      conn
      "SELECT o.id::text, o.href, r.id::text, r.raw_payload \
      \FROM dav_objects o \
      \JOIN dav_object_revisions r ON r.id = o.current_revision_id \
      \LEFT JOIN dav_object_projections p ON p.dav_object_id = o.id \
      \WHERE o.object_kind = 'VCARD' \
      \AND o.deleted_at IS NULL \
      \AND r.raw_payload IS NOT NULL \
      \AND (p.dav_object_id IS NULL \
      \  OR p.parser_version IS DISTINCT FROM ? \
      \  OR p.status <> 'ok')"
      (Only currentParserVersion)
  refreshContactProjectionRows conn rows

refreshContactProjectionRows ::
  Connection ->
  [(Text, Text, Text, ByteString.ByteString)] ->
  IO ()
refreshContactProjectionRows conn rows =
  forM_ rows $ \(objectId, href, revisionId, rawPayload) ->
    case storedContactFromBody href (LazyByteString.fromStrict rawPayload) of
      Right contact -> do
        contactId <- upsertContact conn (storedContactSummary contact)
        replaceContactProjection conn contactId (storedContactSummary contact)
        replaceStructuredContact conn contactId (storedContactProperties contact)
        replaceContactObjectLink conn objectId contactId
        recordDavObjectProjection
          conn
          objectId
          revisionId
          "contact"
          "ok"
          (Just contactId)
          Nothing
          Nothing
      Left problem ->
        recordDavObjectProjection
          conn
          objectId
          revisionId
          "none"
          "failed"
          Nothing
          Nothing
          (Just problem)

refreshStaleCalendarProjections :: Connection -> IO ()
refreshStaleCalendarProjections conn = do
  rows <-
    query
      conn
      "SELECT o.id::text, o.href, r.id::text, r.raw_payload, dc.calendar_id::text \
      \FROM dav_objects o \
      \JOIN dav_object_revisions r ON r.id = o.current_revision_id \
      \JOIN dav_collections dc ON dc.id = o.collection_id \
      \LEFT JOIN dav_object_projections p ON p.dav_object_id = o.id \
      \WHERE o.object_kind IN ('VEVENT', 'VTODO') \
      \AND o.deleted_at IS NULL \
      \AND r.raw_payload IS NOT NULL \
      \AND (p.dav_object_id IS NULL \
      \  OR p.parser_version IS DISTINCT FROM ? \
      \  OR p.status <> 'ok')"
      (Only currentParserVersion)
  refreshCalendarProjectionRows conn rows

refreshCalendarProjectionRows ::
  Connection ->
  [(Text, Text, Text, ByteString.ByteString, Maybe Text)] ->
  IO ()
refreshCalendarProjectionRows conn rows =
  forM_ rows $ \(objectId, href, revisionId, rawPayload, maybeCalendarId) ->
    case storedCalendarFromBody href (LazyByteString.fromStrict rawPayload) of
      Right calendar -> do
        calendarId <-
          case maybeCalendarId of
            Just value -> pure value
            Nothing -> ensureCalendar conn (calendarNameFromHref href)
        componentIds <-
          replaceCalendarProjection
            conn
            calendarId
            objectId
            (storedCalendarProjections calendar)
        case componentIds of
          componentId : _ ->
            recordDavObjectProjection
              conn
              objectId
              revisionId
              "calendar-component"
              "ok"
              Nothing
              (Just componentId)
              Nothing
          [] ->
            recordDavObjectProjection
              conn
              objectId
              revisionId
              "none"
              "skipped"
              Nothing
              Nothing
              Nothing
      Left problem ->
        recordDavObjectProjection
          conn
          objectId
          revisionId
          "none"
          "failed"
          Nothing
          Nothing
          (Just problem)

postgresListContacts :: Backend -> Maybe Text -> IO [ContactSummary]
postgresListContacts config rawQuery = do
  contacts <-
    withStoreConnection config $ \conn -> do
      rows <-
        query_
          conn
          "SELECT c.id::text, o.href, r.raw_payload \
          \FROM dav_objects o \
          \JOIN dav_object_revisions r ON r.id = o.current_revision_id \
          \JOIN contact_dav_objects cdo ON cdo.dav_object_id = o.id \
          \JOIN contacts c ON c.id = cdo.contact_id \
          \WHERE o.object_kind = 'VCARD' \
          \AND o.deleted_at IS NULL \
          \AND r.raw_payload IS NOT NULL \
          \ORDER BY lower(c.display_name), o.href"
      fmap mapMaybeList (mapM (contactSummaryFromRawRow conn) rows)
  pure (filterContacts rawQuery contacts)

mapMaybeList :: [Maybe a] -> [a]
mapMaybeList = mapMaybe id

contactSummaryFromRawRow :: Connection -> (Text, Text, ByteString.ByteString) -> IO (Maybe ContactSummary)
contactSummaryFromRawRow conn (contactId, href, rawPayload) =
  case storedContactFromBody href (LazyByteString.fromStrict rawPayload) of
    Right contact ->
      Just <$> addInferredRelationshipFields conn contactId (storedContactSummary contact)
    Left _ -> pure Nothing

addInferredRelationshipFields :: Connection -> Text -> ContactSummary -> IO ContactSummary
addInferredRelationshipFields conn contactId contact = do
  rows <-
    query
      conn
      "SELECT relationship_label, related_name \
      \FROM contact_relationships \
      \WHERE contact_id = ?::uuid \
      \AND relationship_source = 'inferred' \
      \AND resolution_status = 'resolved' \
      \ORDER BY sort_order, id"
      (Only contactId)
  let inferredFields =
        [ ContactField
            { contactFieldName = "Related"
            , contactFieldLabel = Just labelText
            , contactFieldValue = relatedName
            }
        | (labelText, relatedName) <- rows
        ]
  pure
    contact
      { contactSummaryFields =
          contactSummaryFields contact ++ inferredFields
      }

postgresCreateContact :: Backend -> ContactCreate -> IO ContactSummary
postgresCreateContact config request = do
  contactId <- nextCreatedContactId config
  let href = "/dav/contacts/" <> contactId <> ".vcf"
      body = renderCreatedContact contactId request
  result <- postgresPutContactObject config href body
  case result of
    Left _ -> pure (contactFromCreate contactId request)
    Right _ ->
      case storedContactFromBody href body of
        Right contact -> pure (storedContactSummary contact)
        Left _ -> pure (contactFromCreate contactId request)

nextCreatedContactId :: Backend -> IO Text
nextCreatedContactId config =
  withStoreConnection config $ \conn -> do
    [Only nextNumber] <-
      query_
        conn
        "SELECT count(*)::bigint + 1 \
        \FROM dav_objects \
        \WHERE object_kind = 'VCARD' AND href LIKE '/dav/contacts/contact-%'"
    pure ("contact-" <> Text.pack (show (nextNumber :: Int64)))

postgresListContactObjects :: Backend -> IO [DavObject]
postgresListContactObjects config =
  withStoreConnection config $ \conn -> do
    rows <-
      query_
        conn
        "SELECT o.href, r.etag, r.raw_payload \
        \FROM dav_objects o \
        \JOIN dav_object_revisions r ON r.id = o.current_revision_id \
        \WHERE o.object_kind = 'VCARD' \
        \AND o.deleted_at IS NULL \
        \AND r.raw_payload IS NOT NULL \
        \ORDER BY o.href"
    pure (map contactObjectFromRow rows)

postgresSearchContactObjects :: Backend -> Maybe Text -> IO [DavObject]
postgresSearchContactObjects config rawQuery =
  case Text.toCaseFold . Text.strip <$> rawQuery of
    Just queryText | not (Text.null queryText) ->
      withStoreConnection config $ \conn -> do
        let needle = "%" <> queryText <> "%"
        rows <-
          query
            conn
            "SELECT DISTINCT o.href, r.etag, r.raw_payload \
            \FROM dav_objects o \
            \JOIN dav_object_revisions r ON r.id = o.current_revision_id \
            \JOIN contact_dav_objects cdo ON cdo.dav_object_id = o.id \
            \JOIN contacts c ON c.id = cdo.contact_id \
            \LEFT JOIN contact_emails e ON e.contact_id = c.id \
            \WHERE o.object_kind = 'VCARD' \
            \AND o.deleted_at IS NULL \
            \AND c.deleted_at IS NULL \
            \AND r.raw_payload IS NOT NULL \
            \AND (lower(c.display_name) LIKE ? \
            \OR lower(coalesce(c.vcard_uid, '')) LIKE ? \
            \OR lower(coalesce(e.email::text, '')) LIKE ?) \
            \ORDER BY o.href"
            (needle, needle, needle)
        pure (map contactObjectFromRow rows)
    _ ->
      postgresListContactObjects config

postgresLookupContactObject :: Backend -> Text -> IO (Maybe DavObject)
postgresLookupContactObject config href =
  withStoreConnection config $ \conn -> do
    rows <-
      query
        conn
        "SELECT o.href, r.etag, r.raw_payload \
        \FROM dav_objects o \
        \JOIN dav_object_revisions r ON r.id = o.current_revision_id \
        \WHERE o.object_kind = 'VCARD' AND o.href = ? \
        \AND o.deleted_at IS NULL \
        \AND r.raw_payload IS NOT NULL \
        \LIMIT 1"
        (Only (normalizeDavHref href))
    pure (rowToMaybe contactObjectFromRow rows)

postgresLookupContactObjects :: Backend -> [Text] -> IO [DavObject]
postgresLookupContactObjects _ [] =
  pure []
postgresLookupContactObjects config hrefs =
  withStoreConnection config $ \conn -> do
    rows <-
      query
        conn
        "SELECT o.href, r.etag, r.raw_payload \
        \FROM dav_objects o \
        \JOIN dav_object_revisions r ON r.id = o.current_revision_id \
        \WHERE o.object_kind = 'VCARD' \
        \AND o.href IN ? \
        \AND o.deleted_at IS NULL \
        \AND r.raw_payload IS NOT NULL"
        (Only (In (normalizeDavHref <$> hrefs)))
    pure (map contactObjectFromRow rows)

postgresLookupDavObject :: Backend -> [Text] -> IO (Maybe DavPayload)
postgresLookupDavObject config segments =
  case contactHrefFromLookupSegments segments of
    Just href ->
      fmap DavContact <$> postgresLookupContactRecord config href
    Nothing ->
      case calendarHrefFromLookupSegments segments of
        Just href -> do
          found <- postgresLookupCalendarObject config href
          pure
            ( DavCalendarObject
                . Text.decodeUtf8
                . LazyByteString.toStrict
                . davObjectBody
                <$> found
            )
        Nothing ->
          pure Nothing

postgresLookupContactRecord :: Backend -> Text -> IO (Maybe ContactRecord)
postgresLookupContactRecord config href =
  withStoreConnection config $ \conn -> do
    rows <-
      query
        conn
        "SELECT c.id::text, coalesce(c.vcard_uid, c.id::text), c.display_name \
        \FROM dav_objects o \
        \JOIN contact_dav_objects cdo ON cdo.dav_object_id = o.id \
        \JOIN contacts c ON c.id = cdo.contact_id \
        \WHERE o.object_kind = 'VCARD' \
        \AND o.href = ? \
        \AND o.deleted_at IS NULL \
        \AND c.deleted_at IS NULL \
        \LIMIT 1"
        (Only (normalizeDavHref href))
    case rows of
      [(contactId, contactUid, displayNameText)] -> do
        emails <- contactRecordEmailsFor conn contactId
        relationships <- contactRecordRelationshipsFor conn contactId
        properties <- contactRecordPropertiesFor conn contactId
        pure
          ( Just
              ContactRecord
                { contactRecordProperties = properties
                , contactRecordId = contactUid
                , contactRecordDisplayName = displayNameText
                , contactRecordEmails = emails
                , contactRecordRelationships = relationships
                }
          )
      _ ->
        pure Nothing

contactRecordEmailsFor :: Connection -> Text -> IO [ContactEmail]
contactRecordEmailsFor conn contactId = do
  rows <-
    query
      conn
      "SELECT e.email::text, coalesce(string_agg(t.type, ',' ORDER BY t.type), '') \
      \FROM contact_emails e \
      \LEFT JOIN contact_email_types t ON t.email_id = e.id \
      \WHERE e.contact_id = ?::uuid \
      \GROUP BY e.id, e.email, e.sort_order \
      \ORDER BY e.sort_order, e.email"
      (Only contactId)
  pure
    [ ContactEmail
        { contactEmailAddress = email
        , contactEmailTypes = filter (not . Text.null) (Text.splitOn "," typesText)
        }
    | (email, typesText) <- rows
    ]

contactRecordPropertiesFor :: Connection -> Text -> IO [VCardProperty]
contactRecordPropertiesFor conn contactId = do
  rows <- query conn "SELECT p.property_group, p.property_key, coalesce(p.value, ''), coalesce(jsonb_agg(jsonb_build_array(a.parameter_key, a.parameter_value) ORDER BY a.sort_order, a.id) FILTER (WHERE a.id IS NOT NULL), '[]'::jsonb)::text FROM contact_properties p LEFT JOIN contact_property_parameters a ON a.property_id = p.id WHERE p.contact_id = ?::uuid AND p.property_key ~ '^[A-Z0-9-]+$' GROUP BY p.id ORDER BY p.sort_order, p.id" (Only contactId) :: IO [(Maybe Text, Text, Text, Text)]
  pure [VCardProperty (Text.unpack <$> group) (Text.unpack key) (maybe [] id (Aeson.decodeStrict' (Text.encodeUtf8 parameters))) (Text.unpack value) | (group,key,value,parameters) <- rows]

contactRecordRelationshipsFor :: Connection -> Text -> IO [ContactRelationship]
contactRecordRelationshipsFor conn contactId = do
  rows <-
    query
      conn
      "SELECT r.relationship_label, CASE WHEN r.related_uri IS NOT NULL THEN coalesce(c.display_name, r.related_name) ELSE r.related_name END, r.related_uri FROM contact_relationships r LEFT JOIN contacts c ON c.id = r.related_contact_id WHERE r.contact_id = ?::uuid ORDER BY r.relationship_source, r.sort_order, r.id"
      (Only contactId)
  pure
    [ ContactRelationship
        { relationshipTarget = relatedUri
        , relationshipLabel = label
        , relationshipName = relatedName
        }
    | (label, relatedName, relatedUri) <- rows
    ]

contactObjectFromRow :: (Text, Text, ByteString.ByteString) -> DavObject
contactObjectFromRow (href, etag, rawPayload) =
  DavObject
    { davObjectHref = href
    , davObjectEtag = etag
    , davObjectContentType = "text/vcard; charset=utf-8"
    , davObjectBody = LazyByteString.fromStrict rawPayload
    }

postgresPutContactObject ::
  Backend ->
  Text ->
  LazyByteString.ByteString ->
  IO (Either Text Bool)
postgresPutContactObject config href body =
  case storedContactFromBody href body of
    Left problem -> pure (Left problem)
    Right contact ->
      fmap Right $
        withStoreConnection config $ \conn ->
          withConfigTransaction config conn $ do
            DB.lockCollections conn
            collectionId <- ensureContactCollection conn
            ensureUniqueResourceUid conn collectionId (contactSummaryId (storedContactSummary contact)) (storedContactHref contact)
            existed <- davObjectExists conn collectionId (storedContactHref contact)
            (objectId, revisionId) <-
              upsertDavObject
                conn
                collectionId
                "VCARD"
                (storedContactHref contact)
                (storedContactEtag contact)
                (contactSummaryId (storedContactSummary contact))
                (LazyByteString.toStrict body)
            contactId <- upsertContact conn (storedContactSummary contact)
            clearRelationshipSuppressionsForIncomingRelationships
              conn
              contactId
              (storedContactSummary contact)
            recordRemovedInferredRelationshipSuppressions
              conn
              contactId
              (storedContactSummary contact)
            replaceContactProjection conn contactId (storedContactSummary contact)
            replaceStructuredContact conn contactId (storedContactProperties contact)
            replaceContactObjectLink conn objectId contactId
            recordDavObjectProjection
              conn
              objectId
              revisionId
              "contact"
              "ok"
              (Just contactId)
              Nothing
              Nothing
            refreshAllRelationshipResolutions conn
            pure (not existed)

postgresDeleteContactObject :: Backend -> Text -> IO Bool
postgresDeleteContactObject config href =
  withStoreConnection config $ \conn ->
    withConfigTransaction config conn $ do
      DB.lockCollections conn
      objectRows <-
        query
          conn
          "SELECT id::text, resource_uid \
          \FROM dav_objects \
          \WHERE object_kind = 'VCARD' AND href = ? \
          \AND deleted_at IS NULL \
          \LIMIT 1"
          (Only (normalizeDavHref href))
      case objectRows of
        [(objectId, resourceUid)] -> do
          contactIds <- contactIdsForObject conn objectId
          revisionId <-
            appendDavObjectRevision
              conn
              objectId
              "delete"
              "dav"
              (contentTypeForKind "VCARD")
              "VCARD"
              resourceUid
              (etagForDeletedObject objectId)
              Nothing
              "skipped"
              Nothing
          void $
            execute
              conn
              "UPDATE dav_objects \
              \SET current_revision_id = ?::uuid, deleted_at = now(), \
              \updated_at = now(), deleted_remote = true \
              \WHERE id = ?::uuid"
              (revisionId, objectId)
          void $
            execute
              conn
              "DELETE FROM contact_dav_objects WHERE dav_object_id = ?::uuid"
              (Only objectId)
          recordDavObjectProjection
            conn
            objectId
            revisionId
            "none"
            "skipped"
            Nothing
            Nothing
            Nothing
          forM_ contactIds (deleteOrphanContact conn)
          refreshAllRelationshipResolutions conn
          pure True
        _ ->
          pure False

postgresListCalendars :: Backend -> IO [CalendarSummary]
postgresListCalendars config =
  withStoreConnection config $ \conn -> do
    rows <-
      query_
        conn
        "SELECT display_name, display_name, color, timezone \
        \FROM calendars \
        \WHERE deleted_at IS NULL \
        \ORDER BY lower(display_name)"
    pure (calendarSummaryFromRow <$> rows)

calendarSummaryFromRow :: (Text, Text, Maybe Text, Maybe Text) -> CalendarSummary
calendarSummaryFromRow (calendarId, displayNameText, color, timezone) =
  CalendarSummary
    { calendarSummaryId = calendarId
    , calendarSummaryDisplayName = displayNameText
    , calendarSummaryColor = color
    , calendarSummaryTimezone = timezone
    }

postgresListCalendarEntries ::
  Backend ->
  Maybe Text ->
  IO [CalendarEntrySummary]
postgresListCalendarEntries config rawQuery = do
  entries <-
    withStoreConnection config $ \conn -> do
      rows <-
        query_
          conn
          "SELECT o.href, r.raw_payload \
          \FROM dav_objects o \
          \JOIN dav_object_revisions r ON r.id = o.current_revision_id \
          \WHERE o.object_kind IN ('VEVENT', 'VTODO') \
          \AND o.deleted_at IS NULL \
          \AND r.raw_payload IS NOT NULL \
          \ORDER BY o.href"
      pure (mapMaybe calendarEntrySummaryFromRawRow rows)
  pure (filterCalendarEntries rawQuery entries)

postgresListTodos :: Backend -> Maybe Text -> IO [TodoSummary]
postgresListTodos config rawQuery = do
  todos <-
    withStoreConnection config $ \conn -> do
      rows <-
        query_
          conn
          "SELECT c.ical_uid, cal.display_name, coalesce(c.summary, '(untitled todo)'), \
          \c.status, coalesce(to_char(t.due_at at time zone 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"'), t.due_date::text) \
          \FROM calendar_todos t \
          \JOIN calendar_components c ON c.id = t.component_id \
          \JOIN calendars cal ON cal.id = c.calendar_id \
          \WHERE c.component_kind = 'VTODO' \
          \AND c.deleted_at IS NULL \
          \AND cal.deleted_at IS NULL \
          \ORDER BY coalesce(t.due_at, t.due_date::timestamptz), lower(coalesce(c.summary, ''))"
      pure (todoSummaryFromRow <$> rows)
  pure (filterTodos rawQuery todos)

todoSummaryFromRow :: (Text, Text, Text, Maybe Text, Maybe Text) -> TodoSummary
todoSummaryFromRow (todoId, calendarId, title, status, due) =
  TodoSummary
    { todoSummaryId = todoId
    , todoSummaryCalendarId = calendarId
    , todoSummaryTitle = title
    , todoSummaryStatus = status
    , todoSummaryDue = due
    }

calendarEntrySummaryFromRawRow :: (Text, ByteString.ByteString) -> Maybe CalendarEntrySummary
calendarEntrySummaryFromRawRow (href, rawPayload) =
  case storedCalendarFromBody href (LazyByteString.fromStrict rawPayload) of
    Right calendar -> storedCalendarEntry calendar
    Left _ -> Nothing

postgresListCalendarObjects :: Backend -> IO [DavObject]
postgresListCalendarObjects config =
  withStoreConnection config $ \conn -> do
    rows <-
      query_
        conn
        "SELECT o.href, r.etag, r.raw_payload \
        \FROM dav_objects o \
        \JOIN dav_object_revisions r ON r.id = o.current_revision_id \
        \WHERE o.object_kind IN ('VEVENT', 'VTODO') \
        \AND o.deleted_at IS NULL \
        \AND r.raw_payload IS NOT NULL \
        \ORDER BY o.href"
    pure (map calendarObjectFromRow rows)

postgresListCalendarObjectsInCollection :: Backend -> Text -> IO [DavObject]
postgresListCalendarObjectsInCollection config calendarName =
  withStoreConnection config $ \conn -> do
    rows <-
      query
        conn
        "SELECT o.href, r.etag, r.raw_payload \
        \FROM dav_objects o \
        \JOIN dav_object_revisions r ON r.id = o.current_revision_id \
        \WHERE o.object_kind IN ('VEVENT', 'VTODO') \
        \AND o.href LIKE ? \
        \AND o.deleted_at IS NULL \
        \AND r.raw_payload IS NOT NULL \
        \ORDER BY o.href"
        (Only ("/dav/calendars/" <> calendarName <> "/%" :: Text))
    pure (map calendarObjectFromRow rows)

postgresSearchCalendarObjects ::
  Backend ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  IO [DavObject]
postgresSearchCalendarObjects config maybeCalendarName maybeStart maybeEnd =
  withStoreConnection config $ \conn -> do
    let maybePrefix = calendarPrefix <$> maybeCalendarName
    rows <-
      query
        conn
        "SELECT o.href, r.etag, r.raw_payload \
        \FROM dav_objects o \
        \JOIN dav_object_revisions r ON r.id = o.current_revision_id \
        \WHERE o.object_kind IN ('VEVENT', 'VTODO') \
        \AND (?::text IS NULL OR o.href LIKE ?::text) \
        \AND o.deleted_at IS NULL \
        \AND r.raw_payload IS NOT NULL \
        \ORDER BY o.href"
        ( maybePrefix
        , maybePrefix
        )
    pure
      [ object
      | object <- map calendarObjectFromRow rows
      , ICalendar.calendarObjectMatchesTimeRange
          maybeStart
          maybeEnd
          (Text.decodeUtf8 (LazyByteString.toStrict (davObjectBody object)))
      ]

postgresLookupCalendarObject :: Backend -> Text -> IO (Maybe DavObject)
postgresLookupCalendarObject config href =
  withStoreConnection config $ \conn -> do
    rows <-
      query
        conn
        "SELECT o.href, r.etag, r.raw_payload \
        \FROM dav_objects o \
        \JOIN dav_object_revisions r ON r.id = o.current_revision_id \
        \WHERE o.object_kind IN ('VEVENT', 'VTODO') AND o.href = ? \
        \AND o.deleted_at IS NULL \
        \AND r.raw_payload IS NOT NULL \
        \LIMIT 1"
        (Only (normalizeDavHref href))
    pure (rowToMaybe calendarObjectFromRow rows)

postgresLookupCalendarObjects :: Backend -> [Text] -> IO [DavObject]
postgresLookupCalendarObjects _ [] =
  pure []
postgresLookupCalendarObjects config hrefs =
  withStoreConnection config $ \conn -> do
    rows <-
      query
        conn
        "SELECT o.href, r.etag, r.raw_payload \
        \FROM dav_objects o \
        \JOIN dav_object_revisions r ON r.id = o.current_revision_id \
        \WHERE o.object_kind IN ('VEVENT', 'VTODO') \
        \AND o.href IN ? \
        \AND o.deleted_at IS NULL \
        \AND r.raw_payload IS NOT NULL"
        (Only (In (normalizeDavHref <$> hrefs)))
    pure (map calendarObjectFromRow rows)

calendarObjectFromRow :: (Text, Text, ByteString.ByteString) -> DavObject
calendarObjectFromRow (href, etag, rawPayload) =
  DavObject
    { davObjectHref = href
    , davObjectEtag = etag
    , davObjectContentType = "text/calendar; charset=utf-8"
    , davObjectBody = LazyByteString.fromStrict rawPayload
    }

postgresPutCalendarObject ::
  Backend ->
  Text ->
  LazyByteString.ByteString ->
  IO (Either Text Bool)
postgresPutCalendarObject config href body =
  case storedCalendarFromBody href body of
    Left problem -> pure (Left problem)
    Right calendar ->
      fmap Right $
        withStoreConnection config $ \conn ->
          withConfigTransaction config conn $ do
            DB.lockCollections conn
            calendarId <- ensureCalendar conn (calendarNameFromHref (storedCalendarHref calendar))
            collectionId <-
              ensureCalendarCollection
                conn
                calendarId
                (calendarNameFromHref (storedCalendarHref calendar))
            ensureUniqueResourceUid conn collectionId (storedCalendarUid calendar) (storedCalendarHref calendar)
            existed <- davObjectExists conn collectionId (storedCalendarHref calendar)
            (objectId, revisionId) <-
              upsertDavObject
                conn
                collectionId
                (ICalendar.calendarObjectKindFromBody (strictText body))
                (storedCalendarHref calendar)
                (storedCalendarEtag calendar)
                (storedCalendarUid calendar)
                (LazyByteString.toStrict body)
            componentIds <-
              replaceCalendarProjection
                conn
                calendarId
                objectId
                (storedCalendarProjections calendar)
            case componentIds of
              componentId : _ ->
                recordDavObjectProjection
                  conn
                  objectId
                  revisionId
                  "calendar-component"
                  "ok"
                  Nothing
                  (Just componentId)
                  Nothing
              [] ->
                recordDavObjectProjection
                  conn
                  objectId
                  revisionId
                  "none"
                  "skipped"
                  Nothing
                  Nothing
                  Nothing
            pure (not existed)

postgresDeleteCalendarObject :: Backend -> Text -> IO Bool
postgresDeleteCalendarObject config href =
  withStoreConnection config $ \conn ->
    withConfigTransaction config conn $ do
      DB.lockCollections conn
      objectRows <-
        query
          conn
          "SELECT id::text, object_kind, resource_uid \
          \FROM dav_objects \
          \WHERE object_kind IN ('VEVENT', 'VTODO') AND href = ? \
          \AND deleted_at IS NULL \
          \LIMIT 1"
          (Only (normalizeDavHref href))
      case objectRows of
        [(objectId, objectKind, resourceUid)] -> do
          componentIds <- calendarComponentIdsForObject conn objectId
          revisionId <-
            appendDavObjectRevision
              conn
              objectId
              "delete"
              "dav"
              (contentTypeForKind objectKind)
              objectKind
              resourceUid
              (etagForDeletedObject objectId)
              Nothing
              "skipped"
              Nothing
          void $
            execute
              conn
              "UPDATE dav_objects \
              \SET current_revision_id = ?::uuid, deleted_at = now(), \
              \updated_at = now(), deleted_remote = true \
              \WHERE id = ?::uuid"
              (revisionId, objectId)
          void $
            execute
              conn
              "DELETE FROM calendar_component_dav_objects \
              \WHERE dav_object_id = ?::uuid"
              (Only objectId)
          recordDavObjectProjection
            conn
            objectId
            revisionId
            "none"
            "skipped"
            Nothing
            Nothing
            Nothing
          forM_ componentIds (deleteOrphanCalendarComponent conn)
          pure True
        _ ->
          pure False

ensureLocalAccount :: Connection -> IO Text
ensureLocalAccount conn = do
  found <-
    query
      conn
      "SELECT id::text \
      \FROM dav_accounts \
      \WHERE server_url = ? AND username = ? AND authzid IS NULL \
      \LIMIT 1"
      ("local" :: Text, "mates" :: Text)
  case found of
    [Only accountId] -> pure accountId
    _ -> do
      [Only accountId] <-
        query
          conn
          "INSERT INTO dav_accounts (server_url, username, authzid) \
          \VALUES (?, ?, NULL) \
          \RETURNING id::text"
          ("local" :: Text, "mates" :: Text)
      pure accountId

ensureContactCollection :: Connection -> IO Text
ensureContactCollection conn = do
  accountId <- ensureLocalAccount conn
  ensureCollection conn accountId Nothing "addressbook" "/dav/contacts/" "Contacts"

ensureCalendarCollection :: Connection -> Text -> Text -> IO Text
ensureCalendarCollection conn calendarId calendarName = do
  accountId <- ensureLocalAccount conn
  ensureCollection
    conn
    accountId
    (Just calendarId)
    "calendar"
    ("/dav/calendars/" <> calendarName <> "/")
    calendarName

ensureCollection ::
  Connection ->
  Text ->
  Maybe Text ->
  Text ->
  Text ->
  Text ->
  IO Text
ensureCollection conn accountId calendarId collectionKind url displayNameText = do
  found <-
    query
      conn
      "SELECT id::text \
      \FROM dav_collections \
      \WHERE account_id = ?::uuid AND url = ? \
      \LIMIT 1"
      (accountId, url)
  case found of
    [Only collectionId] -> do
      void $
        execute
          conn
          "UPDATE dav_collections \
          \SET calendar_id = ?::uuid, collection_kind = ?, display_name = coalesce(display_name, ?), \
          \last_seen_at = now() \
          \WHERE id = ?::uuid"
          (calendarId, collectionKind, displayNameText, collectionId)
      pure collectionId
    _ -> do
      [Only collectionId] <-
        query
          conn
          "INSERT INTO dav_collections \
          \(account_id, calendar_id, collection_kind, url, display_name, last_seen_at) \
          \VALUES (?::uuid, ?::uuid, ?, ?, ?, now()) \
          \RETURNING id::text"
          (accountId, calendarId, collectionKind, url, displayNameText)
      pure collectionId

ensureCalendar :: Connection -> Text -> IO Text
ensureCalendar conn calendarName = do
  found <-
    query
      conn
      "SELECT c.id::text FROM calendars c LEFT JOIN dav_collections dc ON dc.calendar_id = c.id WHERE c.deleted_at IS NULL AND (dc.url = ? OR (dc.id IS NULL AND c.display_name = ?)) ORDER BY dc.url NULLS LAST LIMIT 1"
      ("/dav/calendars/" <> calendarName <> "/", calendarName)
  case found of
    [Only calendarId] -> pure calendarId
    _ -> do
      [Only calendarId] <-
        query
          conn
          "INSERT INTO calendars (display_name, timezone) \
          \VALUES (?, ?) \
          \RETURNING id::text"
          (calendarName, "UTC" :: Text)
      pure calendarId

davObjectExists :: Connection -> Text -> Text -> IO Bool
davObjectExists conn collectionId href = do
  rows <-
    query
      conn
      "SELECT id::text \
      \FROM dav_objects \
      \WHERE collection_id = ?::uuid AND href = ? \
      \AND deleted_at IS NULL \
      \LIMIT 1"
      (collectionId, href)
  pure (not (null (rows :: [Only Text])))

upsertDavObject ::
  Connection ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  ByteString.ByteString ->
  IO (Text, Text)
upsertDavObject conn collectionId objectKind href etag resourceUid rawPayload = do
  found <-
    query
      conn
      "SELECT id::text \
      \FROM dav_objects \
      \WHERE collection_id = ?::uuid AND href = ? \
      \LIMIT 1"
      (collectionId, href)
  case found of
    [Only objectId] -> do
      revisionId <-
        appendDavObjectRevision
          conn
          objectId
          "put"
          "dav"
          (contentTypeForKind objectKind)
          objectKind
          (Just resourceUid)
          etag
          (Just rawPayload)
          "ok"
          Nothing
      void $
        execute
          conn
          "UPDATE dav_objects \
          \SET object_kind = ?, current_revision_id = ?::uuid, \
          \resource_uid = ?, updated_at = now(), last_fetched_at = now(), \
          \deleted_at = NULL, deleted_remote = false, deleted_local = false \
          \WHERE id = ?::uuid"
          (objectKind, revisionId, resourceUid, objectId)
      pure (objectId, revisionId)
    _ -> do
      [Only objectId] <-
        query
          conn
          "INSERT INTO dav_objects \
          \(collection_id, object_kind, href, resource_uid, last_fetched_at) \
          \VALUES (?::uuid, ?, ?, ?, now()) \
          \RETURNING id::text"
          (collectionId, objectKind, href, resourceUid)
      revisionId <-
        appendDavObjectRevision
          conn
          objectId
          "put"
          "dav"
          (contentTypeForKind objectKind)
          objectKind
          (Just resourceUid)
          etag
          (Just rawPayload)
          "ok"
          Nothing
      void $
        execute
          conn
          "UPDATE dav_objects \
          \SET current_revision_id = ?::uuid, updated_at = now() \
          \WHERE id = ?::uuid"
          (revisionId, objectId)
      pure (objectId, revisionId)

appendDavObjectRevision ::
  Connection ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  Text ->
  Maybe ByteString.ByteString ->
  Text ->
  Maybe Text ->
  IO Text
appendDavObjectRevision
  conn
  objectId
  operation
  source
  contentType
  objectKind
  resourceUid
  etag
  rawPayload
  parseStatus
  parseError = do
    [Only nextRevisionNo] <-
      query
        conn
        "SELECT coalesce(max(revision_no), 0)::bigint + 1 \
        \FROM dav_object_revisions \
        \WHERE dav_object_id = ?::uuid"
        (Only objectId)
    previousRows <-
      ( query
          conn
          "SELECT current_revision_id::text \
          \FROM dav_objects \
          \WHERE id = ?::uuid \
          \AND current_revision_id IS NOT NULL \
          \LIMIT 1"
          (Only objectId)
          :: IO [Only Text]
      )
    let previousRevisionId =
          case previousRows of
            [Only revisionId] -> Just revisionId
            _ -> Nothing
        rawPayloadBytes =
          Binary <$> rawPayload :: Maybe (Binary ByteString.ByteString)
        rawPayloadSize =
          fromIntegral . ByteString.length <$> rawPayload :: Maybe Int64
    [Only revisionId] <-
      ( query
          conn
          "INSERT INTO dav_object_revisions \
          \(dav_object_id, revision_no, previous_revision_id, operation, source, \
          \content_type, object_kind, resource_uid, raw_payload, \
          \raw_payload_sha256, raw_payload_size, etag, parse_status, \
          \parse_error, parser_version) \
          \VALUES (?::uuid, ?, ?::uuid, ?, ?, ?, ?, ?, ?::bytea, \
          \digest(?::bytea, 'sha256'), ?, ?, ?, ?, ?) \
          \RETURNING id::text"
          ( objectId
          , nextRevisionNo :: Int64
          , previousRevisionId
          , operation
          , source
          , contentType
          , objectKind
          , resourceUid
          , rawPayloadBytes
          , rawPayloadBytes
          , rawPayloadSize
          , etag
          , parseStatus
          , parseError
          , currentParserVersion
          )
          :: IO [Only Text]
      )
    pure revisionId

recordDavObjectProjection ::
  Connection ->
  Text ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  IO ()
recordDavObjectProjection
  conn
  objectId
  revisionId
  projectionKind
  status
  contactId
  calendarComponentId
  errorText =
    void $
      execute
        conn
        "INSERT INTO dav_object_projections \
        \(dav_object_id, revision_id, projection_kind, status, contact_id, \
        \calendar_component_id, error, parser_version, projected_at) \
        \VALUES (?::uuid, ?::uuid, ?, ?, ?::uuid, ?::uuid, ?, ?, now()) \
        \ON CONFLICT (dav_object_id) DO UPDATE \
        \SET revision_id = EXCLUDED.revision_id, \
        \projection_kind = EXCLUDED.projection_kind, \
        \status = EXCLUDED.status, \
        \contact_id = EXCLUDED.contact_id, \
        \calendar_component_id = EXCLUDED.calendar_component_id, \
        \error = EXCLUDED.error, \
        \parser_version = EXCLUDED.parser_version, \
        \projected_at = now()"
        ( objectId
        , revisionId
        , projectionKind
        , status
        , contactId
        , calendarComponentId
        , errorText
        , currentParserVersion
        )

upsertContact :: Connection -> ContactSummary -> IO Text
upsertContact conn contact = do
  found <-
    query
      conn
      "SELECT id::text \
      \FROM contacts \
      \WHERE vcard_uid = ? \
      \LIMIT 1"
      (Only (contactSummaryId contact))
  case found of
    [Only contactId] -> do
      void $
        execute
          conn
          "UPDATE contacts \
          \SET display_name = ?, sort_name = ?, updated_at = now(), deleted_at = NULL \
          \WHERE id = ?::uuid"
          ( contactSummaryDisplayName contact
          , contactSummaryDisplayName contact
          , contactId
          )
      pure contactId
    _ -> do
      [Only contactId] <-
        query
          conn
          "INSERT INTO contacts (vcard_uid, display_name, sort_name) \
          \VALUES (?, ?, ?) \
          \RETURNING id::text"
          ( contactSummaryId contact
          , contactSummaryDisplayName contact
          , contactSummaryDisplayName contact
          )
      pure contactId

replaceContactProjection :: Connection -> Text -> ContactSummary -> IO ()
replaceContactProjection conn contactId contact = do
  void $ execute conn "DELETE FROM contact_emails WHERE contact_id = ?::uuid" (Only contactId)
  void $ execute conn "DELETE FROM contact_properties WHERE contact_id = ?::uuid" (Only contactId)
  void $ execute conn "DELETE FROM contact_dates WHERE contact_id = ?::uuid" (Only contactId)
  void $ execute conn "DELETE FROM contact_notes WHERE contact_id = ?::uuid" (Only contactId)
  void $ execute conn "DELETE FROM contact_media WHERE contact_id = ?::uuid" (Only contactId)
  void $ execute conn "DELETE FROM contact_relationships WHERE contact_id = ?::uuid" (Only contactId)
  forM_ (zip [0 :: Int ..] (contactSummaryEmails contact)) $
    uncurry (insertContactEmail conn contactId)
  forM_ (zip [0 :: Int ..] (contactSummaryFields contact)) $
    uncurry (insertContactField conn contactId)
  forM_ (zip [0 :: Int ..] (contactSummaryFields contact)) $ \(sortOrder, field) ->
    if contactFieldName field == "Related" then
      insertContactRelationship conn contactId sortOrder field
    else
      pure ()
  forM_ (contactSummaryFields contact) $ \field ->
    case contactDateKind field of
      Just kind ->
        insertContactDate conn contactId kind field
      Nothing ->
        pure ()
  forM_ (zip [0 :: Int ..] (contactSummaryFields contact)) $ \(sortOrder, field) ->
    if contactFieldName field == "Note" then
      void $
        execute
          conn
          "INSERT INTO contact_notes (contact_id, note, sort_order) \
          \VALUES (?::uuid, ?, ?)"
          (contactId, contactFieldValue field, sortOrder)
    else
      pure ()
  case contactSummaryPhoto contact of
    Just photo -> insertContactPhoto conn contactId photo
    Nothing -> pure ()

replaceStructuredContact :: Connection -> Text -> [VCardProperty] -> IO ()
replaceStructuredContact conn contactId properties = do
  forM_ ["contact_names", "contact_nicknames", "contact_categories", "contact_organizations", "contact_phones", "contact_addresses", "contact_urls", "contact_impps"] $ \table ->
    void $ execute conn (fromString ("DELETE FROM " ++ table ++ " WHERE contact_id = ?::uuid")) (Only contactId)
  let propertyValue = Text.pack . decodeText . propertyRawValue
      first key = case [propertyValue p | p <- properties, propertyName p == key] of value:_ -> Just value; [] -> Nothing
      types p = [Text.toUpper (Text.strip value) | (key,raw) <- propertyParams p, key == "TYPE", value <- Text.splitOn "," (Text.dropAround (== '"') (Text.pack raw))]
      parameter p key = Text.pack <$> lookup key (propertyParams p)
      label p = parameter p "LABEL"
      components p = take 7 (Text.splitOn ";" (propertyValue p) ++ repeat "")
  void $ execute conn "UPDATE contacts SET kind = ? WHERE id = ?::uuid" (maybe "individual" id (first "KIND"), contactId)
  forM_ (zip [0 :: Int ..] properties) $ \(position,p) -> do
    -- Preserve every original property and parameter, including groups and
    -- extensions, in addition to the normalized query projections.
    [Only propertyId] <- query conn "INSERT INTO contact_properties(contact_id, property_group, property_key, value, value_type, sort_order) VALUES (?::uuid, ?, ?, ?, ?, ?) RETURNING id::text" (contactId,Text.pack <$> propertyGroup p,Text.pack (propertyName p),Text.pack (propertyRawValue p),maybe "text" id (parameter p "VALUE"),position)
    forM_ (zip [0 :: Int ..] (propertyParams p)) $ \(parameterOrder,(key,value)) ->
      void $ execute conn "INSERT INTO contact_property_parameters(property_id, parameter_key, parameter_value, sort_order) VALUES (?::uuid, ?, ?, ?)" (propertyId :: Text,Text.pack key,Text.pack value,parameterOrder)
    case propertyName p of
      "N" -> case components p of
        family:given:additional:prefix:suffix:_ -> void $ execute conn "INSERT INTO contact_names(contact_id, family_name, given_name, additional_names, honorific_prefixes, honorific_suffixes, formatted_name) VALUES (?::uuid, ?, ?, ?, ?, ?, ?)" (contactId,family,given,additional,prefix,suffix,first "FN")
        _ -> pure ()
      "NICKNAME" -> forM_ (Text.splitOn "," (propertyValue p)) $ \value -> void $ execute conn "INSERT INTO contact_nicknames(contact_id, nickname) VALUES (?::uuid, ?) ON CONFLICT DO NOTHING" (contactId,value)
      "CATEGORIES" -> forM_ (Text.splitOn "," (propertyValue p)) $ \value -> void $ execute conn "INSERT INTO contact_categories(contact_id, category) VALUES (?::uuid, ?) ON CONFLICT DO NOTHING" (contactId,value)
      "ORG" -> case components p of
        organization:unit:_ -> void $ execute conn "INSERT INTO contact_organizations(contact_id, organization_name, organization_unit, title, role, sort_order) VALUES (?::uuid, ?, ?, ?, ?, ?)" (contactId,organization,unit,first "TITLE",first "ROLE",position)
        _ -> pure ()
      "TEL" -> do
        [Only phoneId] <- query conn "INSERT INTO contact_phones(contact_id, phone, label, sort_order) VALUES (?::uuid, ?, ?, ?) ON CONFLICT (contact_id, phone) DO UPDATE SET label = EXCLUDED.label RETURNING id::text" (contactId,propertyValue p,label p,position)
        forM_ (types p) $ \value -> void $ execute conn "INSERT INTO contact_phone_types(phone_id,type) VALUES (?::uuid,?) ON CONFLICT DO NOTHING" (phoneId :: Text,value)
      "ADR" -> case components p of
        [box,extended,street,locality,region,postal,country] -> do
          [Only addressId] <- query conn "INSERT INTO contact_addresses(contact_id, label, post_office_box, extended_address, street_address, locality, region, postal_code, country_name, sort_order) VALUES (?::uuid, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id::text" (contactId,label p,box,extended,street,locality,region,postal,country,position)
          forM_ (types p) $ \value -> void $ execute conn "INSERT INTO contact_address_types(address_id,type) VALUES (?::uuid,?) ON CONFLICT DO NOTHING" (addressId :: Text,value)
        _ -> pure ()
      "URL" -> insertEndpoint "contact_urls" "url" "contact_url_types" "url_id" p position
      "IMPP" -> insertEndpoint "contact_impps" "uri" "contact_impp_types" "impp_id" p position
      "RELATED" | (Text.toCaseFold <$> parameter p "VALUE") /= Just "text" ->
        void $ execute conn "UPDATE contact_relationships SET related_uri = ? WHERE contact_id = ?::uuid AND related_name = ? AND relationship_source = 'asserted'" (propertyValue p, contactId, propertyValue p)
      _ -> pure ()
  where
    insertEndpoint table valueColumn typeTable idColumn p position = do
      [Only endpointId] <- query conn (fromString ("INSERT INTO " ++ table ++ "(contact_id," ++ valueColumn ++ ", sort_order) VALUES (?::uuid, ?, ?) ON CONFLICT (contact_id," ++ valueColumn ++ ") DO UPDATE SET sort_order = EXCLUDED.sort_order RETURNING id::text")) (contactId,Text.pack (decodeText (propertyRawValue p)),position)
      forM_ [Text.toUpper value | ("TYPE",raw) <- propertyParams p, value <- Text.splitOn "," (Text.pack raw)] $ \value -> void $ execute conn (fromString ("INSERT INTO " ++ typeTable ++ "(" ++ idColumn ++ ",type) VALUES (?::uuid,?) ON CONFLICT DO NOTHING")) (endpointId :: Text,value)

insertContactEmail :: Connection -> Text -> Int -> ContactEmail -> IO ()
insertContactEmail conn contactId sortOrder email = do
  emailRows <-
    query
      conn
      "INSERT INTO contact_emails \
      \(contact_id, email, is_primary, label, sort_order) \
      \VALUES (?::uuid, ?, ?, ?, ?) \
      \ON CONFLICT (contact_id, email) DO UPDATE \
      \SET is_primary = contact_emails.is_primary OR EXCLUDED.is_primary, \
      \label = coalesce(contact_emails.label, EXCLUDED.label), \
      \sort_order = least(contact_emails.sort_order, EXCLUDED.sort_order) \
      \RETURNING id::text"
      ( contactId
      , contactEmailAddress email
      , sortOrder == 0
      , emailLabel email
      , sortOrder
      )
  case emailRows :: [Only Text] of
    [Only emailId] ->
      forM_ (contactEmailTypes email) $ \emailType ->
        void $
          execute
            conn
            "INSERT INTO contact_email_types (email_id, type) \
            \VALUES (?::uuid, ?) \
            \ON CONFLICT DO NOTHING"
            (emailId, emailType)
    _ -> pure ()

emailLabel :: ContactEmail -> Maybe Text
emailLabel email =
  case contactEmailTypes email of
    [] -> Nothing
    values -> Just (Text.intercalate ", " values)

insertContactField :: Connection -> Text -> Int -> ContactField -> IO ()
insertContactField conn contactId sortOrder field = do
  propertyRows <-
    query
      conn
      "INSERT INTO contact_properties \
      \(contact_id, property_key, value, sort_order) \
      \VALUES (?::uuid, ?, ?, ?) \
      \RETURNING id::text"
      ( contactId
      , contactFieldName field
      , contactFieldValue field
      , sortOrder
      )
  case propertyRows :: [Only Text] of
    [Only propertyId] ->
      case contactFieldLabel field of
        Just labelText ->
          void $
            execute
              conn
              "INSERT INTO contact_property_parameters \
              \(property_id, parameter_key, parameter_value) \
              \VALUES (?::uuid, ?, ?)"
              (propertyId, "LABEL" :: Text, labelText)
        Nothing -> pure ()
    _ -> pure ()

insertContactDate :: Connection -> Text -> Text -> ContactField -> IO ()
insertContactDate conn contactId kind field = do
  let rawValue = Text.strip (contactFieldValue field)
      parsedDate = parseContactDate rawValue
      textValue =
        case parsedDate of
          Just _ -> Nothing
          Nothing -> Just rawValue
      labelText =
        case contactFieldLabel field of
          Just label | not (Text.null (Text.strip label)) -> Just label
          _ -> Just (contactFieldName field)
  if Text.null rawValue then
    pure ()
  else
    void $
      execute
        conn
        "INSERT INTO contact_dates (contact_id, kind, date_value, text_value, label) \
        \VALUES (?::uuid, ?, ?::date, ?, ?)"
        (contactId, kind, parsedDate, textValue, labelText)

contactDateKind :: ContactField -> Maybe Text
contactDateKind field =
  case Text.toCaseFold (Text.strip (contactFieldName field)) of
    "birthday" -> Just "birth"
    "birth date" -> Just "birth"
    "anniversary" -> Just "anniversary"
    "death date" -> Just "death"
    "deathdate" -> Just "death"
    _ -> Nothing

parseContactDate :: Text -> Maybe Day
parseContactDate rawValue =
  let dateText =
        Text.unpack (Text.takeWhile (/= 'T') (Text.strip rawValue))
   in firstParsedDate
        [ parseDateWith "%Y-%m-%d" dateText
        , parseDateWith "%Y%m%d" dateText
        ]

parseDateWith :: String -> String -> Maybe Day
parseDateWith format value =
  parseTimeM True defaultTimeLocale format value

firstParsedDate :: [Maybe Day] -> Maybe Day
firstParsedDate [] =
  Nothing
firstParsedDate (date : rest) =
  case date of
    Just value -> Just value
    Nothing -> firstParsedDate rest

insertContactRelationship :: Connection -> Text -> Int -> ContactField -> IO ()
insertContactRelationship conn contactId sortOrder field =
  void $
    execute
      conn
      "INSERT INTO contact_relationships \
      \(contact_id, relationship_label, related_name, \
      \resolution_status, relationship_source, sort_order) \
      \VALUES (?::uuid, ?, ?, ?, ?, ?)"
      ( contactId
      , canonicalRelationshipLabel (contactFieldLabel field)
      , contactFieldValue field
      , "unresolved" :: Text
      , "asserted" :: Text
      , sortOrder
      )

clearRelationshipSuppressionsForIncomingRelationships ::
  Connection ->
  Text ->
  ContactSummary ->
  IO ()
clearRelationshipSuppressionsForIncomingRelationships conn contactId contact =
  forM_ (incomingRelationshipKeys contact) $ \(label, relatedName) ->
    void $
      execute
        conn
        "DELETE FROM contact_relationship_suppressions \
        \WHERE contact_id = ?::uuid \
        \AND relationship_label = ? \
        \AND lower(related_name) = lower(?::text)"
        (contactId, label, relatedName)

recordRemovedInferredRelationshipSuppressions ::
  Connection ->
  Text ->
  ContactSummary ->
  IO ()
recordRemovedInferredRelationshipSuppressions conn contactId contact = do
  previousInferred <-
    ( query
        conn
        "SELECT relationship_label, related_name, related_contact_id::text \
        \FROM contact_relationships \
        \WHERE contact_id = ?::uuid \
        \AND relationship_source = 'inferred' \
        \AND related_contact_id IS NOT NULL"
        (Only contactId)
        :: IO [(Text, Text, Text)]
    )
  let incomingKeys = incomingRelationshipKeys contact
  forM_ previousInferred $ \(label, relatedName, relatedContactId) ->
    if relationshipKey label relatedName `elem` incomingKeys then
      pure ()
    else
      void $
        execute
          conn
          "INSERT INTO contact_relationship_suppressions \
          \(contact_id, related_contact_id, relationship_label, related_name) \
          \VALUES (?::uuid, ?::uuid, ?, ?) \
          \ON CONFLICT DO NOTHING"
          (contactId, relatedContactId, label, relatedName)

incomingRelationshipKeys :: ContactSummary -> [(Text, Text)]
incomingRelationshipKeys contact =
  [ relationshipKey
      (canonicalRelationshipLabel (contactFieldLabel field))
      (contactFieldValue field)
  | field <- contactSummaryFields contact
  , contactFieldName field == "Related"
  ]

relationshipKey :: Text -> Text -> (Text, Text)
relationshipKey label relatedName =
  (Text.toCaseFold (Text.strip label), Text.toCaseFold (Text.strip relatedName))

canonicalRelationshipLabel :: Maybe Text -> Text
canonicalRelationshipLabel rawLabel =
  case rawLabel >>= standardRelationshipType . Text.toCaseFold . Text.strip of
    Just label -> label
    Nothing -> "contact"

standardRelationshipType :: Text -> Maybe Text
standardRelationshipType label =
  case Text.filter (/= ' ') (Text.replace "_" "-" label) of
    "acquaintance" -> Just "acquaintance"
    "agent" -> Just "agent"
    "assistant" -> Just "assistant"
    "brother" -> Just "brother"
    "child" -> Just "child"
    "colleague" -> Just "colleague"
    "contact" -> Just "contact"
    "co-worker" -> Just "co-worker"
    "coworker" -> Just "co-worker"
    "co-resident" -> Just "co-resident"
    "coresident" -> Just "co-resident"
    "crush" -> Just "crush"
    "date" -> Just "date"
    "daughter" -> Just "daughter"
    "emergency" -> Just "emergency"
    "father" -> Just "father"
    "friend" -> Just "friend"
    "husband" -> Just "husband"
    "kin" -> Just "kin"
    "manager" -> Just "manager"
    "me" -> Just "me"
    "met" -> Just "met"
    "mother" -> Just "mother"
    "neighbor" -> Just "neighbor"
    "parent" -> Just "parent"
    "partner" -> Just "partner"
    "sibling" -> Just "sibling"
    "sister" -> Just "sister"
    "son" -> Just "son"
    "spouse" -> Just "spouse"
    "sweetheart" -> Just "sweetheart"
    "wife" -> Just "wife"
    _ -> Nothing

refreshAllRelationshipResolutions :: Connection -> IO ()
refreshAllRelationshipResolutions conn = do
  before <- relationshipSnapshot conn
  void $
    execute_
      conn
      "DELETE FROM contact_relationships \
      \WHERE relationship_source = 'inferred'"
  assertions <-
    ( query_
        conn
        "SELECT r.id::text, r.contact_id::text, c.display_name, \
        \r.relationship_label, r.related_name, r.sort_order \
        \FROM contact_relationships r \
        \JOIN contacts c ON c.id = r.contact_id \
        \WHERE r.relationship_source = 'asserted' \
        \AND c.deleted_at IS NULL \
        \ORDER BY r.contact_id, r.sort_order, r.id"
        :: IO [(Text, Text, Text, Text, Text, Int)]
    )
  forM_ assertions (resolveRelationshipAssertion conn)
  resolvedAssertions <-
    ( query_
        conn
        "SELECT r.id::text, r.contact_id::text, c.display_name, \
        \r.related_contact_id::text, r.relationship_label, r.sort_order \
        \FROM contact_relationships r \
        \JOIN contacts c ON c.id = r.contact_id \
        \WHERE r.relationship_source = 'asserted' \
        \AND r.resolution_status = 'resolved' \
        \AND r.related_contact_id IS NOT NULL \
        \AND c.deleted_at IS NULL \
        \ORDER BY r.contact_id, r.sort_order, r.id"
        :: IO [(Text, Text, Text, Text, Text, Int)]
    )
  forM_ resolvedAssertions (insertInferredRelationship conn)
  after <- relationshipSnapshot conn
  forM_ (map fst before ++ [i | (i,_) <- after, lookup i before == Nothing]) $ \contactId ->
    if lookup contactId before == lookup contactId after then pure () else do
      objects <- query conn "SELECT dav_object_id::text FROM contact_dav_objects WHERE contact_id = ?::uuid" (Only contactId) :: IO [Only Text]
      forM_ objects $ \(Only objectId) -> void (query conn "SELECT mates_record_change(?::uuid, false)" (Only objectId) :: IO [Only ()])

relationshipSnapshot :: Connection -> IO [(Text,Text)]
relationshipSnapshot conn = query_ conn "SELECT contact_id::text, jsonb_agg(jsonb_build_array(relationship_label, related_name, related_contact_id, related_uri, relationship_source) ORDER BY relationship_label, related_name, relationship_source)::text FROM contact_relationships GROUP BY contact_id"

resolveRelationshipAssertion ::
  Connection ->
  (Text, Text, Text, Text, Text, Int) ->
  IO ()
resolveRelationshipAssertion conn (relationshipId, contactId, _displayName, _label, relatedName, _sortOrder) = do
  uriRows <- query conn "SELECT related_uri FROM contact_relationships WHERE id = ?::uuid" (Only relationshipId) :: IO [Only (Maybe Text)]
  matches <- case uriRows of
    [Only (Just uri)] -> query conn "SELECT c.id::text, c.display_name FROM contacts c JOIN contact_dav_objects l ON l.contact_id = c.id JOIN dav_objects o ON o.id = l.dav_object_id JOIN contact_relationships r ON r.id = ?::uuid WHERE o.deleted_at IS NULL AND c.id <> ?::uuid AND ((r.related_contact_id = c.id) OR (o.href = ? AND (left(?,1) = '/' OR left(?, length(nullif(current_setting('mates.public_url', true), '')) + 1) = nullif(current_setting('mates.public_url', true), '') || '/')))" (relationshipId, contactId, normalizeDavHref uri, uri, uri)
    _ ->
      ( query
          conn
          "SELECT id::text, display_name \
          \FROM contacts \
          \WHERE deleted_at IS NULL \
          \AND id <> ?::uuid \
          \AND lower(display_name) = lower(?::text) \
          \ORDER BY id"
          (contactId, relatedName)
          :: IO [(Text, Text)]
      )
  case matches of
    [(relatedContactId, _)] ->
      void $
        execute
          conn
          "UPDATE contact_relationships \
          \SET related_contact_id = ?::uuid, resolution_status = 'resolved' \
          \WHERE id = ?::uuid"
          (relatedContactId, relationshipId)
    [] ->
      markRelationshipResolution conn relationshipId "unresolved"
    _ ->
      markRelationshipResolution conn relationshipId "ambiguous"

markRelationshipResolution :: Connection -> Text -> Text -> IO ()
markRelationshipResolution conn relationshipId status =
  void $
    execute
      conn
      "UPDATE contact_relationships \
      \SET related_contact_id = NULL, resolution_status = ? \
      \WHERE id = ?::uuid"
      (status, relationshipId)

insertInferredRelationship ::
  Connection ->
  (Text, Text, Text, Text, Text, Int) ->
  IO ()
insertInferredRelationship conn (relationshipId, sourceContactId, sourceName, targetContactId, label, sortOrder) = do
  let inverseLabel = inverseRelationshipLabel label
  reverseAssertions <-
    ( query
        conn
        "SELECT count(*)::bigint \
        \FROM contact_relationships \
        \WHERE contact_id = ?::uuid \
        \AND related_contact_id = ?::uuid \
        \AND relationship_source = 'asserted' \
        \AND resolution_status = 'resolved'"
        (targetContactId, sourceContactId)
        :: IO [Only Int64]
    )
  suppressions <-
    ( query
        conn
        "SELECT count(*)::bigint \
        \FROM contact_relationship_suppressions \
        \WHERE contact_id = ?::uuid \
        \AND related_contact_id = ?::uuid \
        \AND relationship_label = ? \
        \AND lower(related_name) = lower(?::text)"
        (targetContactId, sourceContactId, inverseLabel, sourceName)
        :: IO [Only Int64]
    )
  let hasReverseAssertion =
        case reverseAssertions of
          [Only count] -> count > 0
          _ -> False
      isSuppressed =
        case suppressions of
          [Only count] -> count > 0
          _ -> False
  if hasReverseAssertion || isSuppressed then
    pure ()
  else
    void $
      execute
        conn
        "INSERT INTO contact_relationships \
        \(contact_id, relationship_label, related_name, related_contact_id, \
        \resolution_status, relationship_source, inferred_from_relationship_id, sort_order) \
        \VALUES (?::uuid, ?, ?, ?::uuid, ?, ?, ?::uuid, ?)"
        ( targetContactId
        , inverseLabel
        , sourceName
        , sourceContactId
        , "resolved" :: Text
        , "inferred" :: Text
        , relationshipId
        , sortOrder
        )

inverseRelationshipLabel :: Text -> Text
inverseRelationshipLabel label =
  case Text.toLower label of
    "assistant" -> "manager"
    "brother" -> "sibling"
    "child" -> "parent"
    "contact" -> "contact"
    "daughter" -> "parent"
    "emergency" -> "emergency"
    "father" -> "child"
    "friend" -> "friend"
    "husband" -> "spouse"
    "manager" -> "assistant"
    "mother" -> "child"
    "parent" -> "child"
    "partner" -> "spouse"
    "sibling" -> "sibling"
    "sister" -> "sibling"
    "son" -> "parent"
    "spouse" -> "spouse"
    "wife" -> "spouse"
    _ -> "contact"

insertContactPhoto :: Connection -> Text -> ContactPhoto -> IO ()
insertContactPhoto conn contactId photo = do
  let source = contactPhotoSource photo
      encoded = Text.drop 1 (snd (Text.breakOn "," source))
      decoded = if "data:" `Text.isPrefixOf` source && ";base64," `Text.isInfixOf` source then either (const Nothing) Just (Base64.decode (Text.encodeUtf8 encoded)) else Nothing
      uri = case decoded of Just _ -> Nothing; Nothing -> Just source
  void $ execute conn "INSERT INTO contact_media(contact_id, kind, media_type, uri, data) VALUES (?::uuid, 'PHOTO', ?, ?, ?::bytea)" (contactId, contactPhotoContentType photo, uri, Binary <$> decoded)

replaceContactObjectLink :: Connection -> Text -> Text -> IO ()
replaceContactObjectLink conn objectId contactId = do
  previousContactIds <- contactIdsForObject conn objectId
  void $
    execute
      conn
      "DELETE FROM contact_dav_objects WHERE dav_object_id = ?::uuid"
      (Only objectId)
  void $
    execute
      conn
      "INSERT INTO contact_dav_objects (contact_id, dav_object_id) \
      \VALUES (?::uuid, ?::uuid) \
      \ON CONFLICT (dav_object_id) DO UPDATE SET contact_id = EXCLUDED.contact_id"
      (contactId, objectId)
  forM_ previousContactIds $ \previousContactId ->
    if previousContactId == contactId then
      pure ()
    else
      deleteOrphanContact conn previousContactId

contactIdsForObject :: Connection -> Text -> IO [Text]
contactIdsForObject conn objectId = do
  rows <-
    query
      conn
      "SELECT contact_id::text \
      \FROM contact_dav_objects \
      \WHERE dav_object_id = ?::uuid"
      (Only objectId)
  pure [contactId | Only contactId <- rows]

deleteOrphanContact :: Connection -> Text -> IO ()
deleteOrphanContact conn contactId = do
  rows <-
    query
      conn
      "SELECT count(*)::bigint \
      \FROM contact_dav_objects \
      \WHERE contact_id = ?::uuid"
      (Only contactId)
  case rows of
    [Only count]
      | count == (0 :: Int64) ->
      void $
        execute
          conn
          "DELETE FROM contacts WHERE id = ?::uuid"
          (Only contactId)
    _ -> pure ()

replaceCalendarProjection ::
  Connection ->
  Text ->
  Text ->
  [ICalendar.CalendarProjection] ->
  IO [Text]
replaceCalendarProjection conn calendarId objectId projections = do
  previousComponentIds <- calendarComponentIdsForObject conn objectId
  void $
    execute
      conn
      "DELETE FROM calendar_component_dav_objects WHERE dav_object_id = ?::uuid"
      (Only objectId)
  componentIds <-
    mapM
      (replaceOneCalendarProjection conn calendarId objectId)
      projections
  forM_ previousComponentIds $ \previousComponentId ->
    if previousComponentId `elem` componentIds then
      pure ()
    else
      deleteOrphanCalendarComponent conn previousComponentId
  pure componentIds

replaceOneCalendarProjection ::
  Connection ->
  Text ->
  Text ->
  ICalendar.CalendarProjection ->
  IO Text
replaceOneCalendarProjection conn calendarId objectId projection = do
  componentId <- upsertCalendarComponent conn calendarId projection
  void $
    execute
      conn
      "DELETE FROM calendar_events WHERE component_id = ?::uuid"
      (Only componentId)
  void $
    execute
      conn
      "DELETE FROM calendar_todos WHERE component_id = ?::uuid"
      (Only componentId)
  case ICalendar.projectionEvent projection of
    Just event ->
      replaceCalendarEvent conn componentId event
    Nothing ->
      pure ()
  case ICalendar.projectionTodo projection of
    Just todo ->
      replaceCalendarTodo conn componentId todo
    Nothing ->
      pure ()
  replaceCalendarRecurrence conn componentId projection
  replaceCalendarDetails conn componentId projection
  void $
    execute
      conn
      "INSERT INTO calendar_component_dav_objects \
      \(component_id, dav_object_id) \
      \VALUES (?::uuid, ?::uuid) \
      \ON CONFLICT (component_id, dav_object_id) DO NOTHING"
      (componentId, objectId)
  pure componentId

upsertCalendarComponent ::
  Connection ->
  Text ->
  ICalendar.CalendarProjection ->
  IO Text
upsertCalendarComponent conn calendarId projection = do
  found <-
    query
      conn
      "SELECT id::text \
      \FROM calendar_components \
      \WHERE calendar_id = ?::uuid \
      \AND component_kind = ? \
      \AND ical_uid = ? \
      \AND coalesce(recurrence_id, '') = coalesce(?::text, '') \
      \LIMIT 1"
      ( calendarId
      , ICalendar.projectionKind projection
      , ICalendar.projectionUid projection
      , ICalendar.projectionRecurrenceId projection
      )
  case found of
    [Only componentId] -> do
      void $
        execute
          conn
          "UPDATE calendar_components \
          \SET recurrence_id = ?, summary = ?, description = ?, location = ?, \
          \status = ?, priority = ?, updated_at = now(), deleted_at = NULL \
          \WHERE id = ?::uuid"
          ( ICalendar.projectionRecurrenceId projection
          , ICalendar.projectionSummary projection
          , ICalendar.projectionDescription projection
          , ICalendar.projectionLocation projection
          , ICalendar.projectionStatus projection
          , ICalendar.projectionPriority projection
          , componentId
          )
      pure componentId
    _ -> do
      [Only componentId] <-
        query
          conn
          "INSERT INTO calendar_components \
          \(calendar_id, component_kind, ical_uid, recurrence_id, summary, \
          \description, location, status, priority) \
          \VALUES (?::uuid, ?, ?, ?, ?, ?, ?, ?, ?) \
          \RETURNING id::text"
          ( calendarId
          , ICalendar.projectionKind projection
          , ICalendar.projectionUid projection
          , ICalendar.projectionRecurrenceId projection
          , ICalendar.projectionSummary projection
          , ICalendar.projectionDescription projection
          , ICalendar.projectionLocation projection
          , ICalendar.projectionStatus projection
          , ICalendar.projectionPriority projection
          )
      pure componentId

localTimeText :: Maybe ICalendar.CalendarTimeValue -> Maybe Text
localTimeText (Just (ICalendar.CalendarFloating value)) = Just (Text.pack (show value))
localTimeText _ = Nothing

timeZoneText :: Maybe ICalendar.CalendarTimeValue -> Maybe Text
timeZoneText (Just (ICalendar.CalendarZoned zone _)) = Just zone
timeZoneText _ = Nothing

replaceCalendarEvent :: Connection -> Text -> ICalendar.CalendarEventProjection -> IO ()
replaceCalendarEvent conn componentId event = void $ execute conn
  "INSERT INTO calendar_events(component_id, dtstart_at, dtstart_date, dtstart_local, dtstart_tzid, dtend_at, dtend_date, dtend_local, dtend_tzid, duration, is_all_day) VALUES (?::uuid, ?::timestamptz, ?::date, ?::timestamp, ?, ?::timestamptz, ?::date, ?::timestamp, ?, ?::double precision * interval '1 second', ?)"
  (componentId, ICalendar.calendarTimeValueDateTimeText (ICalendar.eventStarts event), ICalendar.calendarTimeValueDateText (ICalendar.eventStarts event), localTimeText (Just (ICalendar.eventStarts event)), timeZoneText (Just (ICalendar.eventStarts event)), ICalendar.calendarTimeValueDateTimeText =<< ICalendar.eventEnds event, ICalendar.calendarTimeValueDateText =<< ICalendar.eventEnds event, localTimeText (ICalendar.eventEnds event), timeZoneText (ICalendar.eventEnds event), ICalendar.eventDurationSeconds event, ICalendar.eventIsAllDay event)

replaceCalendarTodo :: Connection -> Text -> ICalendar.CalendarTodoProjection -> IO ()
replaceCalendarTodo conn componentId todo = do
  void $
    execute
      conn
      "INSERT INTO calendar_todos \
      \(component_id, dtstart_at, dtstart_date, due_at, due_date, \
      \completed_at, created_at_ical, duration, percent_complete, dtstart_local, due_local, dtstart_tzid, due_tzid) \
      \VALUES (?::uuid, ?::timestamptz, ?::date, ?::timestamptz, ?::date, \
      \?::timestamptz, ?::timestamptz, ?::double precision * interval '1 second', ?, ?::timestamp, ?::timestamp, ?, ?)"
      ( componentId
      , ICalendar.calendarTimeValueDateTimeText =<< ICalendar.todoStarts todo
      , ICalendar.calendarTimeValueDateText =<< ICalendar.todoStarts todo
      , ICalendar.calendarTimeValueDateTimeText =<< ICalendar.todoDue todo
      , ICalendar.calendarTimeValueDateText =<< ICalendar.todoDue todo
      , ICalendar.calendarTimeValueDateTimeText =<< ICalendar.todoCompleted todo
      , ICalendar.calendarTimeValueDateTimeText =<< ICalendar.todoCreated todo
      , ICalendar.todoDurationSeconds todo
      , ICalendar.todoPercentComplete todo
      , localTimeText (ICalendar.todoStarts todo)
      , localTimeText (ICalendar.todoDue todo)
      , timeZoneText (ICalendar.todoStarts todo)
      , timeZoneText (ICalendar.todoDue todo)
      )

replaceCalendarDetails :: Connection -> Text -> ICalendar.CalendarProjection -> IO ()
replaceCalendarDetails conn componentId projection = do
  void $ execute conn "DELETE FROM calendar_properties WHERE component_id = ?::uuid" (Only componentId)
  void $ execute conn "DELETE FROM calendar_participants WHERE component_id = ?::uuid" (Only componentId)
  void $ execute conn "DELETE FROM calendar_alarms WHERE component_id = ?::uuid" (Only componentId)
  forM_ (zip [0 :: Int ..] (ICalendar.projectionOriginalProperties projection)) $ \(position,(key,parameters,value)) -> do
    [Only propertyId] <- query conn "INSERT INTO calendar_properties(component_id, property_key, value, value_type, sort_order) VALUES (?::uuid, ?, ?, ?, ?) RETURNING id::text" (componentId,key,value,maybe "text" id (lookup "VALUE" parameters),position)
    forM_ (zip [0 :: Int ..] parameters) $ \(parameterOrder,(parameterKey,parameterValue)) ->
      void $ execute conn "INSERT INTO calendar_property_parameters(property_id, parameter_key, parameter_value, sort_order) VALUES (?::uuid, ?, ?, ?)" (propertyId :: Text,parameterKey,parameterValue,parameterOrder)
    if key `elem` ["ATTENDEE","ORGANIZER"] then
      void $ execute conn "INSERT INTO calendar_participants(component_id, uri, common_name, role, participation_status, rsvp, participant_kind, sort_order) VALUES (?::uuid, ?, ?, ?, ?, ?, ?, ?)" (componentId,value,lookup "CN" parameters,lookup "ROLE" parameters,lookup "PARTSTAT" parameters,((== "TRUE") . Text.toUpper) <$> lookup "RSVP" parameters,if key == "ORGANIZER" then "organizer" else "attendee" :: Text,position)
    else pure ()
  forM_ (zip [0 :: Int ..] (ICalendar.projectionAlarms projection)) $ \(position,properties) -> do
    let value key = case [v | (k,_,v) <- properties, k == key] of v:_ -> Just v; [] -> Nothing
        triggers = [(p,v) | ("TRIGGER",p,v) <- properties]
    case triggers of
      [(parameters,trigger)] -> do
        let absolute = lookup "VALUE" parameters == Just "DATE-TIME"
        void $ execute conn "INSERT INTO calendar_alarms(component_id, action, trigger_at, trigger_offset, trigger_relation, repeat_count, repeat_interval, summary, description, sort_order) VALUES (?::uuid, ?, ?::timestamptz, ?::double precision * interval '1 second', ?, ?::integer, ?::double precision * interval '1 second', ?, ?, ?)" (componentId,maybe "DISPLAY" id (value "ACTION"),if absolute then Just trigger else Nothing,if absolute then Nothing else ICalendar.parseDurationSeconds trigger,lookup "RELATED" parameters,value "REPEAT",value "DURATION" >>= ICalendar.parseDurationSeconds,value "SUMMARY",value "DESCRIPTION",position)
      _ -> pure ()

replaceCalendarRecurrence ::
  Connection ->
  Text ->
  ICalendar.CalendarProjection ->
  IO ()
replaceCalendarRecurrence conn componentId projection = do
  void $
    execute
      conn
      "DELETE FROM calendar_recurrence_rules WHERE component_id = ?::uuid"
      (Only componentId)
  void $
    execute
      conn
      "DELETE FROM calendar_recurrence_dates WHERE component_id = ?::uuid"
      (Only componentId)
  forM_ (zip [0 :: Int ..] (ICalendar.projectionRecurrenceRules projection)) $ \(sortOrder, rule) ->
    void $
      execute
        conn
        "INSERT INTO calendar_recurrence_rules \
        \(component_id, rule, sort_order) \
        \VALUES (?::uuid, ?, ?)"
        (componentId, rule, sortOrder)
  forM_ (zip [0 :: Int ..] (ICalendar.projectionRecurrenceDates projection)) $ \(sortOrder, recurrenceDate) ->
    void $
      execute
        conn
        "INSERT INTO calendar_recurrence_dates \
        \(component_id, recurrence_kind, occurs_at, occurs_date, sort_order, occurs_local, tzid, period_end_at, period_end_local, period_duration) \
        \VALUES (?::uuid, ?, ?::timestamptz, ?::date, ?, ?::timestamp, ?, ?::timestamptz, ?::timestamp, ?::double precision * interval '1 second')"
        ( componentId
        , ICalendar.recurrenceDateKind recurrenceDate
        , ICalendar.recurrenceDateDateTimeText recurrenceDate
        , ICalendar.recurrenceDateDateText recurrenceDate
        , sortOrder
        , localTimeText (Just (ICalendar.recurrenceDateValue recurrenceDate))
        , timeZoneText (Just (ICalendar.recurrenceDateValue recurrenceDate))
        , ICalendar.calendarTimeValueDateTimeText =<< ICalendar.recurrenceDateEnd recurrenceDate
        , localTimeText (ICalendar.recurrenceDateEnd recurrenceDate)
        , ICalendar.recurrenceDateDurationSeconds recurrenceDate
        )

calendarComponentIdsForObject :: Connection -> Text -> IO [Text]
calendarComponentIdsForObject conn objectId = do
  rows <-
    query
      conn
      "SELECT component_id::text \
      \FROM calendar_component_dav_objects \
      \WHERE dav_object_id = ?::uuid"
      (Only objectId)
  pure [componentId | Only componentId <- rows]

deleteOrphanCalendarComponent :: Connection -> Text -> IO ()
deleteOrphanCalendarComponent conn componentId = do
  rows <-
    query
      conn
      "SELECT count(*)::bigint \
      \FROM calendar_component_dav_objects \
      \WHERE component_id = ?::uuid"
      (Only componentId)
  case rows of
    [Only count]
      | count == (0 :: Int64) ->
      void $
        execute
          conn
          "DELETE FROM calendar_components WHERE id = ?::uuid"
          (Only componentId)
    _ -> pure ()

ensureUniqueResourceUid :: Connection -> Text -> Text -> Text -> IO ()
ensureUniqueResourceUid conn collectionId uid href = do
  rows <- query conn "SELECT href FROM dav_objects WHERE collection_id = ?::uuid AND ((resource_uid = ? AND href <> ?) OR (href = ? AND resource_uid <> ?)) AND deleted_at IS NULL LIMIT 1" (collectionId, uid, href, href, uid) :: IO [Only Text]
  if null rows then pure () else throwIO (DavConflict "no-uid-conflict")

storedCalendarUid :: StoredCalendar -> Text
storedCalendarUid calendar = case storedCalendarProjections calendar of
  projection:_ -> ICalendar.projectionUid projection
  [] -> calendarIdFromHref (storedCalendarHref calendar)

data StoredContact = StoredContact
  { storedContactHref :: Text
  , storedContactEtag :: Text
  , storedContactBody :: LazyByteString.ByteString
  , storedContactProperties :: [VCardProperty]
  , storedContactSummary :: ContactSummary
  }
  deriving (Eq, Show)

data StoredCalendar = StoredCalendar
  { storedCalendarHref :: Text
  , storedCalendarEtag :: Text
  , storedCalendarBody :: LazyByteString.ByteString
  , storedCalendarEntry :: Maybe CalendarEntrySummary
  , storedCalendarProjections :: [ICalendar.CalendarProjection]
  }
  deriving (Eq, Show)

storedContactFromBody ::
  Text ->
  LazyByteString.ByteString ->
  Either Text StoredContact
storedContactFromBody href body =
  case Text.decodeUtf8' (LazyByteString.toStrict body) of
    Left err ->
      Left ("invalid UTF-8 vCard: " <> Text.pack (showUnicodeException err))
    Right bodyText ->
      case parseVCards (Text.unpack bodyText) of
        Right [card] ->
          Right
            StoredContact
              { storedContactHref = normalizeDavHref href
              , storedContactEtag = etagForBody body
              , storedContactBody = body
              , storedContactProperties = parseProperties card
              , storedContactSummary = contactSummaryFromVCard card
              }
        Right [] ->
          Left "no vCard found"
        Right _ ->
          Left "CardDAV object must contain exactly one vCard"
        Left problem ->
          Left ("invalid vCard: " <> Text.pack problem)

validateCalendarObject :: Text -> Either Text ()
validateCalendarObject body =
  let projections = ICalendar.calendarProjectionsFromBody "calendar" "missing-uid" body
      kinds = uniqueTextValues (map ICalendar.projectionKind projections)
      uids = uniqueTextValues (map ICalendar.projectionUid projections)
      identities = [(ICalendar.projectionUid p, ICalendar.projectionRecurrenceId p) | p <- projections]
      hasDuplicate [] = False
      hasDuplicate (value:rest) = value `elem` rest || hasDuplicate rest
      invalidTodo p = case ICalendar.projectionTodo p >>= ICalendar.todoPercentComplete of Just n -> n < 0 || n > 100; Nothing -> False
      invalidDuration p = case ICalendar.projectionEvent p of
        Just event -> maybe False (< 0) (ICalendar.eventDurationSeconds event) || (ICalendar.eventEnds event /= Nothing && ICalendar.eventDurationSeconds event /= Nothing)
        Nothing -> False
   in if null projections || length kinds /= 1 || length uids /= 1 || "missing-uid" `elem` uids || hasDuplicate identities || any invalidTodo projections || any invalidDuration projections
      then Left "Invalid calendar object: require one component kind and UID, unique recurrence instances, and valid timing/status values"
      else Right ()
  where uniqueTextValues = foldr (\value rest -> if value `elem` rest then rest else value:rest) []

storedCalendarFromBody ::
  Text ->
  LazyByteString.ByteString ->
  Either Text StoredCalendar
storedCalendarFromBody href body =
  case Text.decodeUtf8' (LazyByteString.toStrict body) of
    Left err ->
      Left ("invalid UTF-8 iCalendar: " <> Text.pack (showUnicodeException err))
    Right bodyText | Left problem <- Native.normalizeCalendar bodyText -> Left problem
    Right bodyText | Left problem <- validateCalendarObject bodyText -> Left problem
    Right bodyText ->
      if "BEGIN:VCALENDAR" `Text.isInfixOf` bodyText
        then
          let normalizedHref = normalizeDavHref href
              calendarName = calendarNameFromHref normalizedHref
              fallbackUid = calendarIdFromHref normalizedHref
           in Right
                StoredCalendar
                  { storedCalendarHref = normalizedHref
                  , storedCalendarEtag = etagForBody body
                  , storedCalendarBody = body
                  , storedCalendarEntry =
                      ICalendar.calendarEntrySummaryFromBody normalizedHref bodyText
                  , storedCalendarProjections =
                      ICalendar.calendarProjectionsFromBody calendarName fallbackUid bodyText
                  }
        else
          Left "invalid iCalendar: missing BEGIN:VCALENDAR"

calendarNameFromHref :: Text -> Text
calendarNameFromHref href =
  case Text.splitOn "/" (normalizeDavHref href) of
    _ : "dav" : "calendars" : calendarName : _ -> calendarName
    "dav" : "calendars" : calendarName : _ -> calendarName
    _ -> "calendar"

calendarIdFromHref :: Text -> Text
calendarIdFromHref href =
  case reverse (Text.splitOn "/" (normalizeDavHref href)) of
    fileName : _ ->
      maybe fileName id (Text.stripSuffix ".ics" fileName)
    [] ->
      href

calendarPrefix :: Text -> Text
calendarPrefix calendarName =
  "/dav/calendars/" <> calendarName <> "/%"

contactHrefFromLookupSegments :: [Text] -> Maybe Text
contactHrefFromLookupSegments segments =
  case filter (not . Text.null) segments of
    ["contacts", fileName]
      | not (Text.null fileName) ->
          Just ("/dav/contacts/" <> fileName)
    ["dav", "contacts", fileName]
      | not (Text.null fileName) ->
          Just ("/dav/contacts/" <> fileName)
    _ ->
      Nothing

calendarHrefFromLookupSegments :: [Text] -> Maybe Text
calendarHrefFromLookupSegments segments =
  case filter (not . Text.null) segments of
    ["calendars", calendarName, fileName]
      | not (Text.null calendarName) && not (Text.null fileName) ->
          Just ("/dav/calendars/" <> calendarName <> "/" <> fileName)
    ["dav", "calendars", calendarName, fileName]
      | not (Text.null calendarName) && not (Text.null fileName) ->
          Just ("/dav/calendars/" <> calendarName <> "/" <> fileName)
    _ ->
      Nothing

contactSummaryFromVCard :: VCard -> ContactSummary
contactSummaryFromVCard card =
  ContactSummary
    { contactSummaryId = Text.pack (vcardId card)
    , contactSummaryDisplayName = Text.pack (displayName card)
    , contactSummaryEmails =
        [ ContactEmail
            { contactEmailAddress = Text.pack (vcardEmailAddress email)
            , contactEmailTypes = Text.pack <$> vcardEmailTypes email
            }
        | email <- cardEmailAddresses card
        ]
    , contactSummaryFields =
        [ ContactField
            { contactFieldName = Text.pack (vcardFieldName field)
            , contactFieldLabel = Text.pack <$> vcardFieldLabel field
            , contactFieldValue = Text.pack (vcardFieldValue field)
            }
        | field <- cardFields card
        ]
    , contactSummaryPhoto = contactPhotoFromVCard <$> cardPhoto card
    }

contactPhotoFromVCard :: VCardPhoto -> ContactPhoto
contactPhotoFromVCard photo =
  ContactPhoto
    { contactPhotoContentType = Text.pack (vcardPhotoContentType photo)
    , contactPhotoSource = Text.pack (vcardPhotoSource photo)
    }

renderCreatedContact ::
  Text ->
  ContactCreate ->
  LazyByteString.ByteString
renderCreatedContact contactId request =
  LazyByteString.fromStrict . Text.encodeUtf8 . Text.unlines $
    [ "BEGIN:VCARD"
    , "VERSION:4.0"
    , "UID:" <> escapeVCardText contactId
    , "FN:" <> escapeVCardText (Text.strip (contactCreateDisplayName request))
    ]
      <> renderCreatedEmails request
      <> ["END:VCARD"]

renderCreatedEmails :: ContactCreate -> [Text]
renderCreatedEmails request =
  case Text.strip <$> contactCreateEmail request of
    Just email
      | not (Text.null email) ->
          [ "EMAIL"
              <> renderTypeParameters (cleanEmailTypes request)
              <> ":"
              <> escapeVCardText email
          ]
    _ ->
      []

renderTypeParameters :: [Text] -> Text
renderTypeParameters [] =
  ""
renderTypeParameters values =
  ";TYPE=" <> Text.intercalate "," (escapeVCardParameter <$> values)

escapeVCardText :: Text -> Text
escapeVCardText =
  Text.concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar ';' = "\\;"
    escapeChar ',' = "\\,"
    escapeChar '\n' = "\\n"
    escapeChar char = Text.singleton char

escapeVCardParameter :: Text -> Text
escapeVCardParameter =
  Text.filter (`notElem` [';', ':', ','])

contactFromCreate :: Text -> ContactCreate -> ContactSummary
contactFromCreate contactId request =
  ContactSummary
    { contactSummaryId = contactId
    , contactSummaryDisplayName = Text.strip (contactCreateDisplayName request)
    , contactSummaryEmails = createEmails request
    , contactSummaryFields = createFields request
    , contactSummaryPhoto = Nothing
    }

createEmails :: ContactCreate -> [ContactEmail]
createEmails request =
  case Text.strip <$> contactCreateEmail request of
    Just email
      | not (Text.null email) ->
          [ ContactEmail
              { contactEmailAddress = email
              , contactEmailTypes = cleanEmailTypes request
              }
          ]
    _ ->
      []

createFields :: ContactCreate -> [ContactField]
createFields request =
  [ ContactField
      { contactFieldName = "Email"
      , contactFieldLabel = Just (Text.intercalate ", " (cleanEmailTypes request))
      , contactFieldValue = email
      }
  | email <- maybeToList (Text.strip <$> contactCreateEmail request)
  , not (Text.null email)
  ]

cleanEmailTypes :: ContactCreate -> [Text]
cleanEmailTypes request =
  case filter (not . Text.null) (Text.strip <$> contactCreateEmailTypes request) of
    [] -> ["INTERNET"]
    values -> values

filterContacts :: Maybe Text -> [ContactSummary] -> [ContactSummary]
filterContacts Nothing contacts =
  contacts
filterContacts (Just rawQuery) contacts =
  let
    queryText =
      Text.toCaseFold (Text.strip rawQuery)
  in
  if Text.null queryText
    then contacts
    else filter (matchesContact queryText) contacts

matchesContact :: Text -> ContactSummary -> Bool
matchesContact queryText contact =
  queryText `Text.isInfixOf` Text.toCaseFold (contactSummaryDisplayName contact)
    || queryText `Text.isInfixOf` Text.toCaseFold (contactSummaryId contact)
    || any (emailMatches queryText) (contactSummaryEmails contact)
    || any (fieldMatches queryText) (contactSummaryFields contact)

emailMatches :: Text -> ContactEmail -> Bool
emailMatches queryText email =
  queryText `Text.isInfixOf` Text.toCaseFold (contactEmailAddress email)

fieldMatches :: Text -> ContactField -> Bool
fieldMatches queryText field =
  any
    (queryText `Text.isInfixOf`)
    ( Text.toCaseFold
        <$> [ contactFieldName field
            , maybe "" id (contactFieldLabel field)
            , contactFieldValue field
            ]
    )

filterCalendarEntries ::
  Maybe Text ->
  [CalendarEntrySummary] ->
  [CalendarEntrySummary]
filterCalendarEntries Nothing entries =
  entries
filterCalendarEntries (Just rawQuery) entries =
  let
    queryText =
      Text.toCaseFold (Text.strip rawQuery)
  in
  if Text.null queryText
    then entries
    else filter (matchesCalendarEntry queryText) entries

matchesCalendarEntry :: Text -> CalendarEntrySummary -> Bool
matchesCalendarEntry queryText entry =
  any
    (queryText `Text.isInfixOf`)
    ( Text.toCaseFold
        <$> [ calendarEntryId entry
            , calendarEntryTitle entry
            , calendarEntryCalendarName entry
            , maybe "" id (calendarEntryLocation entry)
            , maybe "" id (calendarEntryDescription entry)
            ]
    )

filterTodos :: Maybe Text -> [TodoSummary] -> [TodoSummary]
filterTodos Nothing todos =
  todos
filterTodos (Just rawQuery) todos =
  let queryText = Text.toCaseFold (Text.strip rawQuery)
   in if Text.null queryText
        then todos
        else filter (matchesTodo queryText) todos

matchesTodo :: Text -> TodoSummary -> Bool
matchesTodo queryText todo =
  any
    (queryText `Text.isInfixOf`)
    ( Text.toCaseFold
        <$> [ todoSummaryId todo
            , todoSummaryCalendarId todo
            , todoSummaryTitle todo
            , maybe "" id (todoSummaryStatus todo)
            , maybe "" id (todoSummaryDue todo)
            ]
    )

normalizeDavHref :: Text -> Text
normalizeDavHref href =
  let
    stripped = href
    pathOnly =
      case Text.breakOn "://" stripped of
        (_, "") ->
          stripped
        (_, rest) ->
          case Text.drop 3 rest of
            withoutScheme ->
              case Text.dropWhile (/= '/') withoutScheme of
                "" -> "/"
                path -> path
    absolute =
      if "/" `Text.isPrefixOf` pathOnly
        then pathOnly
        else "/" <> pathOnly
  in
  Text.decodeUtf8With lenientDecode (urlDecode False (Text.encodeUtf8 absolute))

etagForBody :: LazyByteString.ByteString -> Text
etagForBody = Digest.etag

etagForBytes :: ByteString.ByteString -> Text
etagForBytes =
  etagForBody . LazyByteString.fromStrict

etagForDeletedObject :: Text -> Text
etagForDeletedObject objectId =
  "\"mates-deleted-" <> objectId <> "\""

contentTypeForKind :: Text -> Text
contentTypeForKind "VCARD" =
  "text/vcard; charset=utf-8"
contentTypeForKind _ =
  "text/calendar; charset=utf-8"

currentParserVersion :: Text
currentParserVersion =
  "mates-2026-10-architecture-2"

showUnicodeException :: UnicodeException -> String
showUnicodeException =
  show

strictText :: LazyByteString.ByteString -> Text
strictText =
  Text.decodeUtf8 . LazyByteString.toStrict

rowToMaybe :: (a -> b) -> [a] -> Maybe b
rowToMaybe _ [] = Nothing
rowToMaybe f (value : _) = Just (f value)
