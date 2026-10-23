{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (when)
import System.Environment (getArgs)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , close
  , connectPostgreSQL
  , query
  , query_
  )
import Mates.CardDAV (RemoteCard (..), parseAddressBookHrefs, parseRemoteCards)
import Mates.Config (Config (..), parseConfig, renderConfig)
import Mates.Server.ClientProfile (ClientProfile (..))
import Mates.Server.Render (renderContactVCard, renderContactVCardFromRaw)
import Mates.Server.Store
  ( PostgresConfig (..)
  , Store (..)
  , newPostgresStore
  , resetPostgresStore
  )
import Mates.Server.Types
  ( CalendarEntrySummary (..)
  , CalendarSummary (..)
  , ContactCreate (..)
  , ContactEmail (..)
  , ContactField (..)
  , ContactRecord (..)
  , ContactRelationship (..)
  , ContactSummary (..)
  , DavObject (..)
  , DavPayload (..)
  , TodoSummary (..)
  )
import Mates.VCard
  ( VCard (..)
  , VCardEmail (..)
  , VCardField (..)
  , VCardPhoto (..)
  , cardEmailAddresses
  , cardFields
  , cardPhoto
  , matchesVCardQuery
  , parseVCards
  , propertyValues
  , renderHumanReadable
  , vcardId
  )

main :: IO ()
main = do
  args <- getArgs
  when (args /= ["--database"]) $ do
    testVCardParsing
    testGeneratedLocalId
    testConfigRoundTrip
    testAuthzIdConfigRoundTrip
    testLegacyAuthConfig
    testMinimalConfig
    testRemoteCardParsing
    testAddressBookDiscoveryParsing
    testHumanReadableRendering
    testVCardGenealogyFields
    testVCardMatchingAndEmailRows
    testClientSpecificRelationshipRendering
  when (args /= ["--unit"]) $ do
    testPostgresContactCreation
    testPostgresGenealogyFacts
    testPostgresRelationshipResolution
    testPostgresInferredRelationshipSuppression
    testPostgresAmbiguousRelationshipResolution
    testCardDavObjectStore
    testDavObjectSearchAndScopedLookup
    testCalDavObjectStore
    testPostgresStoreStartsEmpty
  putStrLn "mates-test: ok"

testVCardParsing :: IO ()
testVCardParsing =
  case parseVCards sampleCard of
    Right [card] -> do
      assertEqual "uid" (Just "friend-1") (cardUid card)
      assertEqual "fn" (Just "Ada Lovelace") (cardFormattedName card)
      assertEqual "email" ["ada@example.test"] (cardEmails card)
      assertEqual "tel" ["+44 20 0000 0000"] (cardPhones card)
      assertEqual
        "folded note"
        ["first linecontinued"]
        (propertyValues "NOTE" (lines (cardRaw card)))
    other -> fail ("unexpected parse result: " ++ show other)

testGeneratedLocalId :: IO ()
testGeneratedLocalId =
  case parseVCards sampleCardWithoutUid of
    Right [card] ->
      assertBool "local id prefix" ("local-" `isPrefixOf` vcardId card)
    other -> fail ("unexpected parse result: " ++ show other)

testConfigRoundTrip :: IO ()
testConfigRoundTrip = do
  let config =
        Config
          { configServer = "https://carddav.example.test"
          , configUser = "ada"
          , configAddressBook = Just "/addressbooks/ada/default/"
          , configAuthzId = Nothing
          }
  assertEqual "config round trip" (Right config) (parseConfig (renderConfig config))

testAuthzIdConfigRoundTrip :: IO ()
testAuthzIdConfigRoundTrip = do
  let config =
        Config
          { configServer = "https://carddav.example.test"
          , configUser = "ada"
          , configAddressBook = Nothing
          , configAuthzId = Just "contacts-admin@EXAMPLE.TEST"
          }
  assertEqual
    "authzid config round trip"
    (Right config)
    (parseConfig (renderConfig config))

testLegacyAuthConfig :: IO ()
testLegacyAuthConfig = do
  let raw =
        unlines
          [ "server=https://carddav.example.test"
          , "user=ada"
          , "auth=gssapi"
          , "gssapi-authzid=contacts-admin@EXAMPLE.TEST"
          ]
      expected =
        Config
          { configServer = "https://carddav.example.test"
          , configUser = "ada"
          , configAddressBook = Nothing
          , configAuthzId = Just "contacts-admin@EXAMPLE.TEST"
          }
  assertEqual "legacy auth config" (Right expected) (parseConfig raw)

testMinimalConfig :: IO ()
testMinimalConfig = do
  let raw =
        unlines
          [ "server=https://carddav.example.test"
          , "user=ada"
          ]
      expected =
        Config
          { configServer = "https://carddav.example.test"
          , configUser = "ada"
          , configAddressBook = Nothing
          , configAuthzId = Nothing
          }
  assertEqual "minimal config" (Right expected) (parseConfig raw)

testRemoteCardParsing :: IO ()
testRemoteCardParsing =
  case parseRemoteCards sampleMultiStatus of
    Right [remote] -> do
      assertEqual "remote href" "/cards/ada/friend-1.vcf" (remoteHref remote)
      assertEqual "remote etag" (Just "\"123\"") (remoteEtag remote)
      assertEqual "remote uid" (Just "friend-1") (cardUid (remoteCard remote))
      assertEqual
        "remote fn"
        (Just "Ada Lovelace")
        (cardFormattedName (remoteCard remote))
    other -> fail ("unexpected remote parse result: " ++ show other)

testAddressBookDiscoveryParsing :: IO ()
testAddressBookDiscoveryParsing =
  assertEqual
    "addressbook discovery hrefs"
    ["/elric/contacts/"]
    (parseAddressBookHrefs sampleAddressBookDiscovery)

testHumanReadableRendering :: IO ()
testHumanReadableRendering =
  case parseVCards sampleRichCard of
    Right [card] -> do
      let rendered = renderHumanReadable card
      assertContains "rendered name" "Vera Dowdeswell" rendered
      assertContains "rendered email" "home: vera@example.test" rendered
      assertContains "rendered daughter" "daughter: Bethany Christine Dowdeswell" rendered
      assertContains "rendered image note" "Image: present (not displayed)" rendered
      assertNotContains "hidden photo data" "BASE64PHOTO" rendered
      assertNotContains "hidden apple label" "_$!<daughter>!$_" rendered
      assertEqual
        "photo data uri"
        ( Just
            VCardPhoto
              { vcardPhotoContentType = "image/jpeg"
              , vcardPhotoSource = "data:image/jpeg;base64,BASE64PHOTO"
              }
        )
        (cardPhoto card)
    other -> fail ("unexpected rich card parse result: " ++ show other)

testVCardGenealogyFields :: IO ()
testVCardGenealogyFields =
  case parseVCards sampleGenealogyCard of
    Right [card] -> do
      let fields = cardFields card
          rendered = renderHumanReadable card
      assertBool
        "genealogy fields include death date"
        ( VCardField
            { vcardFieldName = "Death date"
            , vcardFieldLabel = Nothing
            , vcardFieldValue = "1852-11-27"
            }
            `elem` fields
        )
      assertBool
        "genealogy fields include birth place"
        ( VCardField
            { vcardFieldName = "Birth place"
            , vcardFieldLabel = Nothing
            , vcardFieldValue = "London, England"
            }
            `elem` fields
        )
      assertBool
        "genealogy fields include death place"
        ( VCardField
            { vcardFieldName = "Death place"
            , vcardFieldLabel = Nothing
            , vcardFieldValue = "Marylebone, England"
            }
            `elem` fields
        )
      assertContains "rendered death date" "Death date: 1852-11-27" rendered
      assertContains "rendered birth place" "Birth place: London, England" rendered
    other -> fail ("unexpected genealogy card parse result: " ++ show other)

testVCardMatchingAndEmailRows :: IO ()
testVCardMatchingAndEmailRows =
  case parseVCards sampleCard of
    Right [card] -> do
      assertBool "partial name match" (matchesVCardQuery "love" card)
      assertBool "partial email match" (matchesVCardQuery "example" card)
      assertBool "case-insensitive email match" (matchesVCardQuery "ADA@" card)
      assertBool "query miss" (not (matchesVCardQuery "hopper" card))
      assertEqual
        "email rows"
        [ VCardEmail
            { vcardEmailAddress = "ada@example.test"
            , vcardEmailTypes = ["INTERNET", "HOME"]
            }
        ]
        (cardEmailAddresses card)
      assertBool
        "detail fields include phone"
        ( VCardField
            { vcardFieldName = "Phone"
            , vcardFieldLabel = Just "cell"
            , vcardFieldValue = "+44 20 0000 0000"
            }
            `elem` cardFields card
        )
    other -> fail ("unexpected parse result: " ++ show other)

testClientSpecificRelationshipRendering :: IO ()
testClientSpecificRelationshipRendering = do
  let contact =
        ContactRecord
          { contactRecordProperties = []
          , contactRecordId = "vera-1"
          , contactRecordDisplayName = "Vera Dowdeswell"
          , contactRecordEmails =
              [ ContactEmail
                  { contactEmailAddress = "vera@example.test"
                  , contactEmailTypes = ["HOME", "INTERNET"]
                  }
              ]
          , contactRecordRelationships =
              [ ContactRelationship
                  { relationshipTarget = Nothing
                  , relationshipLabel = "daughter"
                  , relationshipName = "Bethany Christine Dowdeswell"
                  }
              ]
          }
      apple = renderContactVCard AppleContacts contact
      generic = renderContactVCard GenericDAV contact
      rawApple =
        Text.unlines
          [ "BEGIN:VCARD"
          , "VERSION:4.0"
          , "UID:vera-1"
          , "FN:Vera Dowdeswell"
          , "TEL;TYPE=cell:+44 20 0000 0000"
          , "item1.X-ABRELATEDNAMES:Bethany Christine Dowdeswell"
          , "item1.X-ABLabel:_$!<daughter>!$_"
          , "END:VCARD"
          ]
      presentedGeneric = renderContactVCardFromRaw GenericDAV rawApple contact
      presentedApple = renderContactVCardFromRaw AppleContacts rawApple contact
  assertContainsText "apple related name" "item1.X-ABRELATEDNAMES" apple
  assertContainsText "apple related label" "item1.X-ABLabel:_$!<daughter>!$_" apple
  assertContainsText "generic related name" "RELATED;TYPE=daughter;VALUE=text" generic
  assertContainsText "raw phone preserved" "TEL;TYPE=cell:+44 20 0000 0000" presentedGeneric
  assertContainsText "generic raw related name" "RELATED;TYPE=daughter;VALUE=text" presentedGeneric
  assertNotContainsText "generic strips apple relation" "X-ABRELATEDNAMES" presentedGeneric
  assertContainsText "apple raw relation" "item1.X-ABRELATEDNAMES" presentedApple
  assertContainsText "apple raw specific label" "item1.X-ABLabel:_$!<daughter>!$_" presentedApple

testPostgresContactCreation :: IO ()
testPostgresContactCreation = do
  store <- newTestStore
  created <-
    storeCreateContact
      store
      ContactCreate
        { contactCreateDisplayName = "Ada Lovelace"
        , contactCreateEmail = Just "ada@example.test"
        , contactCreateEmailTypes = ["INTERNET", "HOME"]
        }
  assertEqual "created id" "contact-1" (contactSummaryId created)
  assertEqual "created name" "Ada Lovelace" (contactSummaryDisplayName created)
  assertEqual
    "created email"
    [ ContactEmail
        { contactEmailAddress = "ada@example.test"
        , contactEmailTypes = ["INTERNET", "HOME"]
        }
    ]
    (contactSummaryEmails created)
  assertEqual
    "created fields"
    [ ContactField
        { contactFieldName = "Email"
        , contactFieldLabel = Just "internet, home"
        , contactFieldValue = "ada@example.test"
        }
    ]
    (contactSummaryFields created)
  assertEqual "stored contact" [created] =<< storeListContacts store Nothing
  assertEqual "name query" [created] =<< storeListContacts store (Just "love")
  assertEqual "email query" [created] =<< storeListContacts store (Just "example")
  assertEqual "query miss" [] =<< storeListContacts store (Just "hopper")

testPostgresGenealogyFacts :: IO ()
testPostgresGenealogyFacts = do
  store <- newTestStore
  assertPutContact store "/dav/contacts/ada-genealogy.vcf" sampleGenealogyCard
  dateRows <-
    withTestConnection $ \conn ->
      ( query_
          conn
          "SELECT d.kind, coalesce(d.date_value::text, ''), \
          \coalesce(d.text_value, ''), coalesce(d.label, '') \
          \FROM contact_dates d \
          \JOIN contacts c ON c.id = d.contact_id \
          \WHERE c.vcard_uid = 'ada-genealogy' \
          \ORDER BY d.kind"
          :: IO [(Text.Text, Text.Text, Text.Text, Text.Text)]
      )
  assertEqual
    "projected genealogy dates"
    [("birth", "1815-12-10", "", "Birthday"), ("death", "1852-11-27", "", "Death date")]
    dateRows
  placeRows <-
    withTestConnection $ \conn ->
      ( query_
          conn
          "SELECT p.property_key, p.value \
          \FROM contact_properties p \
          \JOIN contacts c ON c.id = p.contact_id \
          \WHERE c.vcard_uid = 'ada-genealogy' \
          \AND p.property_key IN ('Birth place', 'Death place') \
          \ORDER BY p.property_key"
          :: IO [(Text.Text, Text.Text)]
      )
  assertEqual
    "projected genealogy places"
    [("Birth place", "London, England"), ("Death place", "Marylebone, England")]
    placeRows
  relationshipRows <-
    withTestConnection $ \conn ->
      ( query_
          conn
          "SELECT r.relationship_label, r.related_name \
          \FROM contact_relationships r \
          \JOIN contacts c ON c.id = r.contact_id \
          \WHERE c.vcard_uid = 'ada-genealogy' \
          \ORDER BY r.relationship_label, r.related_name"
          :: IO [(Text.Text, Text.Text)]
      )
  assertEqual
    "specific genealogy relationship labels are preserved"
    [("father", "Lord Byron"), ("mother", "Annabella Milbanke")]
    relationshipRows

testPostgresRelationshipResolution :: IO ()
testPostgresRelationshipResolution = do
  store <- newTestStore
  assertPutContact store "/dav/contacts/vera.vcf" sampleVeraDaughterCard
  assertPutContact store "/dav/contacts/bethany.vcf" sampleBethanyCard
  relationships <-
    withTestConnection $ \conn ->
      ( query_
          conn
          "SELECT source.display_name, r.relationship_label, r.related_name, \
          \coalesce(target.display_name, ''), r.resolution_status, r.relationship_source \
          \FROM contact_relationships r \
          \JOIN contacts source ON source.id = r.contact_id \
          \LEFT JOIN contacts target ON target.id = r.related_contact_id \
          \ORDER BY source.display_name, r.relationship_source, r.relationship_label"
          :: IO
              [ ( Text.Text
                , Text.Text
                , Text.Text
                , Text.Text
                , Text.Text
                , Text.Text
                )
              ]
      )
  assertEqual
    "resolved relationship rows"
    [ ( "Bethany Christine Dowdeswell"
      , "parent"
      , "Vera Dowdeswell"
      , "Vera Dowdeswell"
      , "resolved"
      , "inferred"
      )
    , ( "Vera Dowdeswell"
      , "daughter"
      , "Bethany Christine Dowdeswell"
      , "Bethany Christine Dowdeswell"
      , "resolved"
      , "asserted"
      )
    ]
    relationships
  normalizedPayload <- storeLookupDavObject store ["contacts", "vera.vcf"]
  case normalizedPayload of
    Just (DavContact contact) ->
      assertEqual
        "normalized contact relationship label"
        [ ContactRelationship
            { relationshipTarget = Nothing
                  , relationshipLabel = "daughter"
            , relationshipName = "Bethany Christine Dowdeswell"
            }
        ]
        (contactRecordRelationships contact)
    other ->
      fail ("unexpected normalized DAV payload: " ++ show other)
  contacts <- storeListContacts store Nothing
  case [ contactSummaryFields contact
       | contact <- contacts
       , contactSummaryDisplayName contact == "Bethany Christine Dowdeswell"
       ] of
    [fields] ->
      assertBool
        "inferred relationship is visible in contact summary"
        ( ContactField
            { contactFieldName = "Related"
            , contactFieldLabel = Just "parent"
            , contactFieldValue = "Vera Dowdeswell"
            }
            `elem` fields
        )
    other -> fail ("unexpected Bethany contact fields: " ++ show other)

testPostgresInferredRelationshipSuppression :: IO ()
testPostgresInferredRelationshipSuppression = do
  store <- newTestStore
  assertPutContact store "/dav/contacts/alice.vcf" sampleAliceSiblingCard
  assertPutContact store "/dav/contacts/beatrice.vcf" sampleBeatriceCard
  before <- storeLookupDavObject store ["contacts", "beatrice.vcf"]
  case before of
    Just (DavContact contact) ->
      assertEqual
        "inferred sibling exists before edit"
        [ ContactRelationship
            { relationshipTarget = Nothing
                  , relationshipLabel = "sibling"
            , relationshipName = "Alice Sibling"
            }
        ]
        (contactRecordRelationships contact)
    other ->
      fail ("unexpected Beatrice contact before suppression: " ++ show other)
  assertPutContact store "/dav/contacts/beatrice.vcf" sampleBeatriceCard
  after <- storeLookupDavObject store ["contacts", "beatrice.vcf"]
  case after of
    Just (DavContact contact) ->
      assertEqual
        "deleted inferred sibling stays suppressed"
        []
        (contactRecordRelationships contact)
    other ->
      fail ("unexpected Beatrice contact after suppression: " ++ show other)
  suppressions <-
    withTestConnection $ \conn ->
      ( query_
          conn
          "SELECT target.display_name, s.relationship_label, s.related_name, source.display_name \
          \FROM contact_relationship_suppressions s \
          \JOIN contacts target ON target.id = s.contact_id \
          \JOIN contacts source ON source.id = s.related_contact_id \
          \ORDER BY target.display_name, s.relationship_label, s.related_name"
          :: IO [(Text.Text, Text.Text, Text.Text, Text.Text)]
      )
  assertEqual
    "stored inferred relationship suppression"
    [("Beatrice Sibling", "sibling", "Alice Sibling", "Alice Sibling")]
    suppressions

testPostgresAmbiguousRelationshipResolution :: IO ()
testPostgresAmbiguousRelationshipResolution = do
  store <- newTestStore
  assertPutContact store "/dav/contacts/vera.vcf" sampleVeraDaughterCard
  assertPutContact store "/dav/contacts/bethany-1.vcf" sampleBethanyCard
  assertPutContact store "/dav/contacts/bethany-2.vcf" sampleSecondBethanyCard
  statuses <-
    withTestConnection $ \conn ->
      ( query_
          conn
          "SELECT r.resolution_status, r.related_contact_id IS NULL \
          \FROM contact_relationships r \
          \JOIN contacts source ON source.id = r.contact_id \
          \WHERE source.display_name = 'Vera Dowdeswell' \
          \AND r.relationship_source = 'asserted'"
          :: IO [(Text.Text, Bool)]
      )
  assertEqual "ambiguous relationship is not linked" [("ambiguous", True)] statuses
  inferredCount <-
    withTestConnection $ \conn ->
      ( query
          conn
          "SELECT count(*)::bigint \
          \FROM contact_relationships \
          \WHERE relationship_source = ?"
          (Only ("inferred" :: Text.Text))
          :: IO [Only Int]
      )
  assertEqual "ambiguous relationship has no inferred inverse" [Only 0] inferredCount

testCardDavObjectStore :: IO ()
testCardDavObjectStore = do
  store <- newTestStore
  created <-
    storePutContactObject
      store
      "/dav/contacts/friend-1.vcf"
      (LazyChar8.pack sampleCard)
  assertEqual "dav put created" (Right True) created
  objects <- storeListContactObjects store
  assertEqual "dav object count" 1 (length objects)
  assertEqual "dav href" ["/dav/contacts/friend-1.vcf"] (davObjectHref <$> objects)
  assertBool "dav etag" (all (not . Text.null . davObjectEtag) objects)
  assertEqual
    "dav contact list"
    ["Ada Lovelace"]
    . map contactSummaryDisplayName
    =<< storeListContacts store Nothing
  summaries <- storeListContacts store Nothing
  case summaries of
    summary : _ ->
      assertEqual
        "dav detail fields"
        ["Email", "Phone", "Note"]
        (map contactFieldName (contactSummaryFields summary))
    [] ->
      fail "expected stored contact summary"
  updated <-
    storePutContactObject
      store
      "/dav/contacts/friend-1.vcf"
      (LazyChar8.pack sampleCard)
  assertEqual "dav put updated" (Right False) updated
  removed <- storeDeleteContactObject store "/dav/contacts/friend-1.vcf"
  assertEqual "dav delete" True removed
  assertEqual "dav object deleted" [] =<< storeListContactObjects store
  contactRevisionRows <-
    withTestConnection $ \conn ->
      ( query
          conn
          "SELECT r.operation, r.raw_payload IS NOT NULL \
          \FROM dav_objects o \
          \JOIN dav_object_revisions r ON r.dav_object_id = o.id \
          \WHERE o.href = ? \
          \ORDER BY r.revision_no"
          (Only ("/dav/contacts/friend-1.vcf" :: Text.Text))
          :: IO [(Text.Text, Bool)]
      )
  assertEqual
    "dav contact revisions"
    [("put", True), ("put", True), ("delete", False)]
    contactRevisionRows
  contactTombstoneRows <-
    withTestConnection $ \conn ->
      ( query
          conn
          "SELECT deleted_at IS NOT NULL, current_revision_id IS NOT NULL \
          \FROM dav_objects \
          \WHERE href = ?"
          (Only ("/dav/contacts/friend-1.vcf" :: Text.Text))
          :: IO [(Bool, Bool)]
      )
  assertEqual "dav contact tombstone" [(True, True)] contactTombstoneRows

testDavObjectSearchAndScopedLookup :: IO ()
testDavObjectSearchAndScopedLookup = do
  store <- newTestStore
  assertPutContact store "/dav/contacts/friend-1.vcf" sampleCard
  assertPutContact store "/dav/contacts/grace.vcf" sampleGraceCard
  adaObjects <- storeSearchContactObjects store (Just "ada")
  assertEqual
    "searched contact href"
    ["/dav/contacts/friend-1.vcf"]
    (davObjectHref <$> adaObjects)
  emailObjects <- storeSearchContactObjects store (Just "example.test")
  assertEqual
    "searched contacts by partial email"
    ["/dav/contacts/friend-1.vcf", "/dav/contacts/grace.vcf"]
    (davObjectHref <$> emailObjects)
  lookedUpContacts <-
    storeLookupContactObjects
      store
      ["/dav/contacts/missing.vcf", "/dav/contacts/grace.vcf"]
  assertEqual
    "multi contact lookup"
    ["/dav/contacts/grace.vcf"]
    (davObjectHref <$> lookedUpContacts)
  _ <-
    storePutCalendarObject
      store
      "/dav/calendars/imported/planning.ics"
      (LazyChar8.pack sampleCalendar)
  _ <-
    storePutCalendarObject
      store
      "/dav/calendars/imported/all-day.ics"
      (LazyChar8.pack sampleAllDayCalendar)
  _ <-
    storePutCalendarObject
      store
      "/dav/calendars/imported/later.ics"
      (LazyChar8.pack sampleCalendarLater)
  _ <-
    storePutCalendarObject
      store
      "/dav/calendars/imported/recurring.ics"
      (LazyChar8.pack sampleRecurringCalendar)
  _ <-
    storePutCalendarObject
      store
      "/dav/calendars/imported/todo-due.ics"
      (LazyChar8.pack sampleTodoDue)
  _ <-
    storePutCalendarObject
      store
      "/dav/calendars/imported/todo-created-completed.ics"
      (LazyChar8.pack sampleTodoCreatedCompleted)
  _ <-
    storePutCalendarObject
      store
      "/dav/calendars/imported/todo-start-duration.ics"
      (LazyChar8.pack sampleTodoStartDuration)
  _ <-
    storePutCalendarObject
      store
      "/dav/calendars/work/planning.ics"
      (LazyChar8.pack sampleCalendarWork)
  importedObjects <- storeListCalendarObjectsInCollection store "imported"
  assertEqual
    "scoped calendar collection"
    [ "/dav/calendars/imported/all-day.ics"
    , "/dav/calendars/imported/later.ics"
    , "/dav/calendars/imported/planning.ics"
    , "/dav/calendars/imported/recurring.ics"
    , "/dav/calendars/imported/todo-created-completed.ics"
    , "/dav/calendars/imported/todo-due.ics"
    , "/dav/calendars/imported/todo-start-duration.ics"
    ]
    (davObjectHref <$> importedObjects)
  rangedObjects <-
    storeSearchCalendarObjects
      store
      (Just "imported")
      (Just "2026-05-08T00:00:00Z")
      (Just "2026-05-09T00:00:00Z")
  assertEqual
    "calendar time-range in collection"
    [ "/dav/calendars/imported/all-day.ics"
    , "/dav/calendars/imported/planning.ics"
    ]
    (davObjectHref <$> rangedObjects)
  globalRangedObjects <-
    storeSearchCalendarObjects
      store
      Nothing
      (Just "2026-05-09T00:00:00Z")
      (Just "2026-05-10T00:00:00Z")
  assertEqual
    "calendar global time-range"
    ["/dav/calendars/work/planning.ics"]
    (davObjectHref <$> globalRangedObjects)
  movedRecurringObjects <-
    storeSearchCalendarObjects
      store
      (Just "imported")
      (Just "2026-05-24T13:00:00Z")
      (Just "2026-05-24T14:00:00Z")
  assertEqual
    "calendar recurring override time-range"
    ["/dav/calendars/imported/recurring.ics"]
    (davObjectHref <$> movedRecurringObjects)
  originalRecurringObjects <-
    storeSearchCalendarObjects
      store
      (Just "imported")
      (Just "2026-05-22T09:00:00Z")
      (Just "2026-05-22T10:00:00Z")
  assertEqual
    "calendar recurring overridden original skipped"
    []
    (davObjectHref <$> originalRecurringObjects)
  todoDueObjects <-
    storeSearchCalendarObjects
      store
      (Just "imported")
      (Just "2026-05-12T00:00:00Z")
      (Just "2026-05-13T00:00:00Z")
  assertEqual
    "vtodo due-only time-range"
    ["/dav/calendars/imported/todo-due.ics"]
    (davObjectHref <$> todoDueObjects)
  todoCompletedObjects <-
    storeSearchCalendarObjects
      store
      (Just "imported")
      (Just "2026-05-16T09:00:00Z")
      (Just "2026-05-16T11:00:00Z")
  assertEqual
    "vtodo created completed time-range"
    ["/dav/calendars/imported/todo-created-completed.ics"]
    (davObjectHref <$> todoCompletedObjects)
  todoDurationObjects <-
    storeSearchCalendarObjects
      store
      (Just "imported")
      (Just "2026-05-13T10:00:00Z")
      (Just "2026-05-13T10:30:00Z")
  assertEqual
    "vtodo dtstart duration time-range"
    ["/dav/calendars/imported/todo-start-duration.ics"]
    (davObjectHref <$> todoDurationObjects)
  todos <- storeListTodos store Nothing
  assertEqual
    "projected vtodo summaries"
    ["todo-due-1", "todo-created-completed-1", "todo-start-duration-1"]
    (todoSummaryId <$> todos)
  lookedUpCalendars <-
    storeLookupCalendarObjects
      store
      ["/dav/calendars/work/planning.ics", "/dav/calendars/missing/planning.ics"]
  assertEqual
    "multi calendar lookup"
    ["/dav/calendars/work/planning.ics"]
    (davObjectHref <$> lookedUpCalendars)

testCalDavObjectStore :: IO ()
testCalDavObjectStore = do
  store <- newTestStore
  created <-
    storePutCalendarObject
      store
      "/dav/calendars/imported/planning.ics"
      (LazyChar8.pack sampleCalendar)
  assertEqual "caldav put created" (Right True) created
  objects <- storeListCalendarObjects store
  assertEqual "caldav object count" 1 (length objects)
  assertEqual
    "caldav href"
    ["/dav/calendars/imported/planning.ics"]
    (davObjectHref <$> objects)
  assertBool "caldav etag" (all (not . Text.null . davObjectEtag) objects)
  calendars <- storeListCalendars store
  assertBool
    "imported calendar visible"
    ("imported" `elem` (calendarSummaryId <$> calendars))
  entries <- storeListCalendarEntries store (Just "planning")
  assertEqual "imported event count" 1 (length entries)
  assertEqual "imported event id" ["event-ics-1"] (calendarEntryId <$> entries)
  assertEqual
    "imported event start"
    ["2026-05-08T09:00:00Z"]
    (calendarEntryStartsAt <$> entries)
  updated <-
    storePutCalendarObject
      store
      "/dav/calendars/imported/planning.ics"
      (LazyChar8.pack sampleCalendar)
  assertEqual "caldav put updated" (Right False) updated
  removed <- storeDeleteCalendarObject store "/dav/calendars/imported/planning.ics"
  assertEqual "caldav delete" True removed
  assertEqual "caldav object deleted" [] =<< storeListCalendarObjects store
  calendarRevisionRows <-
    withTestConnection $ \conn ->
      ( query
          conn
          "SELECT r.operation, r.raw_payload IS NOT NULL \
          \FROM dav_objects o \
          \JOIN dav_object_revisions r ON r.dav_object_id = o.id \
          \WHERE o.href = ? \
          \ORDER BY r.revision_no"
          (Only ("/dav/calendars/imported/planning.ics" :: Text.Text))
          :: IO [(Text.Text, Bool)]
      )
  assertEqual
    "caldav object revisions"
    [("put", True), ("put", True), ("delete", False)]
    calendarRevisionRows

testPostgresStoreStartsEmpty :: IO ()
testPostgresStoreStartsEmpty = do
  store <- newTestStore
  calendars <- storeListCalendars store
  entries <- storeListCalendarEntries store Nothing
  contacts <- storeListContacts store Nothing
  assertEqual "empty postgres contacts" 0 (length contacts)
  assertEqual "empty postgres calendars" 0 (length calendars)
  assertEqual "empty postgres entries" 0 (length entries)

testPostgresConfig :: PostgresConfig
testPostgresConfig =
  PostgresConfig {postgresConnectionString = "dbname=mates_test"}

newTestStore :: IO Store
newTestStore = do
  resetPostgresStore testPostgresConfig
  newPostgresStore testPostgresConfig

withTestConnection :: (Connection -> IO a) -> IO a
withTestConnection =
  bracket
    (connectPostgreSQL (postgresConnectionString testPostgresConfig))
    close

assertPutContact :: Store -> Text.Text -> String -> IO ()
assertPutContact store href body = do
  result <- storePutContactObject store href (LazyChar8.pack body)
  case result of
    Right _ -> pure ()
    Left problem -> fail ("unexpected contact PUT failure: " ++ Text.unpack problem)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  if expected == actual
    then pure ()
    else fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

assertBool :: String -> Bool -> IO ()
assertBool label condition =
  if condition
    then pure ()
    else fail (label ++ ": condition was false")

assertContains :: String -> String -> String -> IO ()
assertContains label needle haystack =
  assertBool label (needle `isInfixOf` haystack)

assertNotContains :: String -> String -> String -> IO ()
assertNotContains label needle haystack =
  assertBool label (not (needle `isInfixOf` haystack))

assertContainsText :: String -> Text.Text -> Text.Text -> IO ()
assertContainsText label needle haystack =
  assertBool label (needle `Text.isInfixOf` haystack)

assertNotContainsText :: String -> Text.Text -> Text.Text -> IO ()
assertNotContainsText label needle haystack =
  assertBool label (not (needle `Text.isInfixOf` haystack))

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack =
  any (needle `isPrefixOf`) (tails haystack)

tails :: [a] -> [[a]]
tails [] = [[]]
tails values@(_ : rest) = values : tails rest

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (expected : expectedRest) (actual : actualRest) =
  expected == actual && isPrefixOf expectedRest actualRest

sampleCard :: String
sampleCard =
  unlines
    [ "BEGIN:VCARD"
    , "VERSION:4.0"
    , "UID:friend-1"
    , "FN:Ada Lovelace"
    , "EMAIL;TYPE=internet,home:ada@example.test"
    , "TEL;TYPE=cell:+44 20 0000 0000"
    , "NOTE:first line"
    , " continued"
    , "END:VCARD"
    ]

sampleCardWithoutUid :: String
sampleCardWithoutUid =
  unlines
    [ "BEGIN:VCARD"
    , "VERSION:4.0"
    , "FN:Grace Hopper"
    , "END:VCARD"
    ]

sampleGraceCard :: String
sampleGraceCard =
  unlines
    [ "BEGIN:VCARD"
    , "VERSION:4.0"
    , "UID:grace-1"
    , "FN:Grace Hopper"
    , "EMAIL;TYPE=internet,work:grace@example.test"
    , "END:VCARD"
    ]

sampleRichCard :: String
sampleRichCard =
  unlines
    [ "BEGIN:VCARD"
    , "VERSION:4.0"
    , "UID:vera-1"
    , "FN:Vera Dowdeswell"
    , "EMAIL;TYPE=home:vera@example.test"
    , "TEL;TYPE=cell:+44 20 0000 0000"
    , "PHOTO;ENCODING=b;TYPE=JPEG:BASE64PHOTO"
    , "item4.X-ABRELATEDNAMES;type=pref:Bethany Christine Dowdeswell"
    , "item4.X-ABLabel:_$!<daughter>!$_"
    , "NOTE:first line"
    , "END:VCARD"
    ]

sampleGenealogyCard :: String
sampleGenealogyCard =
  unlines
    [ "BEGIN:VCARD"
    , "VERSION:4.0"
    , "UID:ada-genealogy"
    , "FN:Ada Lovelace"
    , "BDAY:18151210"
    , "DEATHDATE:1852-11-27"
    , "BIRTHPLACE:London\\, England"
    , "DEATHPLACE:Marylebone\\, England"
    , "RELATED;TYPE=mother;VALUE=text:Annabella Milbanke"
    , "RELATED;TYPE=father;VALUE=text:Lord Byron"
    , "NOTE:Potential genealogy seed."
    , "END:VCARD"
    ]

sampleVeraDaughterCard :: String
sampleVeraDaughterCard =
  unlines
    [ "BEGIN:VCARD"
    , "VERSION:4.0"
    , "UID:vera-relationship"
    , "FN:Vera Dowdeswell"
    , "item1.X-ABRELATEDNAMES:Bethany Christine Dowdeswell"
    , "item1.X-ABLabel:_$!<daughter>!$_"
    , "END:VCARD"
    ]

sampleBethanyCard :: String
sampleBethanyCard =
  unlines
    [ "BEGIN:VCARD"
    , "VERSION:4.0"
    , "UID:bethany-relationship-1"
    , "FN:Bethany Christine Dowdeswell"
    , "END:VCARD"
    ]

sampleSecondBethanyCard :: String
sampleSecondBethanyCard =
  unlines
    [ "BEGIN:VCARD"
    , "VERSION:4.0"
    , "UID:bethany-relationship-2"
    , "FN:Bethany Christine Dowdeswell"
    , "END:VCARD"
    ]

sampleAliceSiblingCard :: String
sampleAliceSiblingCard =
  unlines
    [ "BEGIN:VCARD"
    , "VERSION:4.0"
    , "UID:alice-sibling"
    , "FN:Alice Sibling"
    , "RELATED;TYPE=sister;VALUE=text:Beatrice Sibling"
    , "END:VCARD"
    ]

sampleBeatriceCard :: String
sampleBeatriceCard =
  unlines
    [ "BEGIN:VCARD"
    , "VERSION:4.0"
    , "UID:beatrice-sibling"
    , "FN:Beatrice Sibling"
    , "END:VCARD"
    ]

sampleMultiStatus :: String
sampleMultiStatus =
  unlines
    [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    , "<D:multistatus xmlns:D=\"DAV:\" xmlns:C=\"urn:ietf:params:xml:ns:carddav\">"
    , "  <D:response>"
    , "    <D:href>/cards/ada/friend-1.vcf</D:href>"
    , "    <D:propstat>"
    , "      <D:prop>"
    , "        <D:getetag>&quot;123&quot;</D:getetag>"
    , "        <C:address-data>"
        ++ "BEGIN:VCARD&#13;&#10;"
        ++ "VERSION:4.0&#13;&#10;"
        ++ "UID:friend-1&#13;&#10;"
        ++ "FN:Ada Lovelace&#13;&#10;"
        ++ "EMAIL:ada@example.test&#13;&#10;"
        ++ "END:VCARD&#13;&#10;"
        ++ "</C:address-data>"
    , "      </D:prop>"
    , "      <D:status>HTTP/1.1 200 OK</D:status>"
    , "    </D:propstat>"
    , "  </D:response>"
    , "</D:multistatus>"
    ]

sampleAddressBookDiscovery :: String
sampleAddressBookDiscovery =
  unlines
    [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    , "<D:multistatus xmlns:D=\"DAV:\" xmlns:C=\"urn:ietf:params:xml:ns:carddav\">"
    , "  <D:response>"
    , "    <D:href>/elric/</D:href>"
    , "    <D:propstat>"
    , "      <D:prop><D:resourcetype><D:collection/></D:resourcetype></D:prop>"
    , "      <D:status>HTTP/1.1 200 OK</D:status>"
    , "    </D:propstat>"
    , "  </D:response>"
    , "  <D:response>"
    , "    <D:href>/elric/contacts/</D:href>"
    , "    <D:propstat>"
    , "      <D:prop>"
    , "        <D:resourcetype><D:collection/><C:addressbook/></D:resourcetype>"
    , "      </D:prop>"
    , "      <D:status>HTTP/1.1 200 OK</D:status>"
    , "    </D:propstat>"
    , "  </D:response>"
    , "</D:multistatus>"
    ]

sampleCalendar :: String
sampleCalendar =
  unlines
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , "PRODID:-//mates//test//EN"
    , "BEGIN:VEVENT"
    , "UID:event-ics-1"
    , "SUMMARY:Planning session"
    , "DTSTART:20260508T090000Z"
    , "DTEND:20260508T103000Z"
    , "LOCATION:Video call"
    , "DESCRIPTION:Review CalDAV sync path."
    , "END:VEVENT"
    , "END:VCALENDAR"
    ]

sampleAllDayCalendar :: String
sampleAllDayCalendar =
  unlines
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , "PRODID:-//mates//test//EN"
    , "BEGIN:VEVENT"
    , "UID:event-ics-all-day-1"
    , "SUMMARY:All day planning"
    , "DTSTART;VALUE=DATE:20260508"
    , "DTEND;VALUE=DATE:20260509"
    , "END:VEVENT"
    , "END:VCALENDAR"
    ]

sampleCalendarLater :: String
sampleCalendarLater =
  unlines
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , "PRODID:-//mates//test//EN"
    , "BEGIN:VEVENT"
    , "UID:event-ics-later-1"
    , "SUMMARY:Later planning"
    , "DTSTART:20260601T090000Z"
    , "DTEND:20260601T100000Z"
    , "END:VEVENT"
    , "END:VCALENDAR"
    ]

sampleRecurringCalendar :: String
sampleRecurringCalendar =
  unlines
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , "PRODID:-//mates//test//EN"
    , "BEGIN:VEVENT"
    , "UID:event-ics-recurring-1"
    , "SUMMARY:Weekly planning"
    , "DTSTART:20260515T090000Z"
    , "DTEND:20260515T100000Z"
    , "RRULE:FREQ=WEEKLY;COUNT=2"
    , "END:VEVENT"
    , "BEGIN:VEVENT"
    , "UID:event-ics-recurring-1"
    , "RECURRENCE-ID:20260522T090000Z"
    , "SUMMARY:Moved weekly planning"
    , "DTSTART:20260524T130000Z"
    , "DTEND:20260524T140000Z"
    , "END:VEVENT"
    , "END:VCALENDAR"
    ]

sampleTodoDue :: String
sampleTodoDue =
  unlines
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , "PRODID:-//mates//test//EN"
    , "BEGIN:VTODO"
    , "UID:todo-due-1"
    , "SUMMARY:Pay renewal"
    , "DUE:20260512T120000Z"
    , "END:VTODO"
    , "END:VCALENDAR"
    ]

sampleTodoCreatedCompleted :: String
sampleTodoCreatedCompleted =
  unlines
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , "PRODID:-//mates//test//EN"
    , "BEGIN:VTODO"
    , "UID:todo-created-completed-1"
    , "SUMMARY:Complete archival task"
    , "CREATED:20260515T080000Z"
    , "COMPLETED:20260516T100000Z"
    , "END:VTODO"
    , "END:VCALENDAR"
    ]

sampleTodoStartDuration :: String
sampleTodoStartDuration =
  unlines
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , "PRODID:-//mates//test//EN"
    , "BEGIN:VTODO"
    , "UID:todo-start-duration-1"
    , "SUMMARY:Write deployment notes"
    , "DTSTART:20260513T090000Z"
    , "DURATION:PT2H"
    , "END:VTODO"
    , "END:VCALENDAR"
    ]

sampleCalendarWork :: String
sampleCalendarWork =
  unlines
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , "PRODID:-//mates//test//EN"
    , "BEGIN:VEVENT"
    , "UID:event-ics-work-1"
    , "SUMMARY:Work planning"
    , "DTSTART:20260509T090000Z"
    , "DTEND:20260509T100000Z"
    , "END:VEVENT"
    , "END:VCALENDAR"
    ]
