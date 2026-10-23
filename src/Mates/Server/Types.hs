{-# LANGUAGE OverloadedStrings #-}

module Mates.Server.Types
  ( CalendarEntrySummary (..)
  , CalendarSummary (..)
  , ContactCreate (..)
  , ContactEmail (..)
  , ContactField (..)
  , ContactPhoto (..)
  , ContactRecord (..)
  , ContactRelationship (..)
  , ContactSummary (..)
  , DavObject (..)
  , DavPayload (..)
  , TodoSummary (..)
  )
where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.!=)
  , (.=)
  )
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Text (Text)
import Mates.VCard (VCardProperty)

data ContactEmail = ContactEmail
  { contactEmailAddress :: Text
  , contactEmailTypes :: [Text]
  }
  deriving (Eq, Show)

data ContactField = ContactField
  { contactFieldName :: Text
  , contactFieldLabel :: Maybe Text
  , contactFieldValue :: Text
  }
  deriving (Eq, Show)

data ContactPhoto = ContactPhoto
  { contactPhotoContentType :: Text
  , contactPhotoSource :: Text
  }
  deriving (Eq, Show)

data ContactRelationship = ContactRelationship
  { relationshipTarget :: Maybe Text
  , relationshipLabel :: Text
  , relationshipName :: Text
  }
  deriving (Eq, Show)

data ContactSummary = ContactSummary
  { contactSummaryId :: Text
  , contactSummaryDisplayName :: Text
  , contactSummaryEmails :: [ContactEmail]
  , contactSummaryFields :: [ContactField]
  , contactSummaryPhoto :: Maybe ContactPhoto
  }
  deriving (Eq, Show)

data ContactCreate = ContactCreate
  { contactCreateDisplayName :: Text
  , contactCreateEmail :: Maybe Text
  , contactCreateEmailTypes :: [Text]
  }
  deriving (Eq, Show)

data ContactRecord = ContactRecord
  { contactRecordProperties :: [VCardProperty]
  , contactRecordId :: Text
  , contactRecordDisplayName :: Text
  , contactRecordEmails :: [ContactEmail]
  , contactRecordRelationships :: [ContactRelationship]
  }
  deriving (Eq, Show)

data CalendarSummary = CalendarSummary
  { calendarSummaryId :: Text
  , calendarSummaryDisplayName :: Text
  , calendarSummaryColor :: Maybe Text
  , calendarSummaryTimezone :: Maybe Text
  }
  deriving (Eq, Show)

data CalendarEntrySummary = CalendarEntrySummary
  { calendarEntryId :: Text
  , calendarEntryCalendarId :: Text
  , calendarEntryCalendarName :: Text
  , calendarEntryTitle :: Text
  , calendarEntryStartsAt :: Text
  , calendarEntryEndsAt :: Maybe Text
  , calendarEntryLocation :: Maybe Text
  , calendarEntryDescription :: Maybe Text
  , calendarEntryAllDay :: Bool
  }
  deriving (Eq, Show)

data TodoSummary = TodoSummary
  { todoSummaryId :: Text
  , todoSummaryCalendarId :: Text
  , todoSummaryTitle :: Text
  , todoSummaryStatus :: Maybe Text
  , todoSummaryDue :: Maybe Text
  }
  deriving (Eq, Show)

data DavObject = DavObject
  { davObjectHref :: Text
  , davObjectEtag :: Text
  , davObjectContentType :: Text
  , davObjectBody :: LazyByteString.ByteString
  }
  deriving (Eq, Show)

data DavPayload
  = DavContact ContactRecord
  | DavCalendarObject Text
  deriving (Eq, Show)

instance ToJSON ContactEmail where
  toJSON email =
    object
      [ "email" .= contactEmailAddress email
      , "types" .= contactEmailTypes email
      ]

instance ToJSON ContactField where
  toJSON field =
    object
      [ "name" .= contactFieldName field
      , "label" .= contactFieldLabel field
      , "value" .= contactFieldValue field
      ]

instance ToJSON ContactPhoto where
  toJSON photo =
    object
      [ "contentType" .= contactPhotoContentType photo
      , "source" .= contactPhotoSource photo
      ]

instance ToJSON ContactSummary where
  toJSON contact =
    object
      [ "id" .= contactSummaryId contact
      , "displayName" .= contactSummaryDisplayName contact
      , "emails" .= contactSummaryEmails contact
      , "fields" .= contactSummaryFields contact
      , "photo" .= contactSummaryPhoto contact
      ]

instance FromJSON ContactCreate where
  parseJSON =
    withObject "ContactCreate" $ \value ->
      ContactCreate
        <$> value .: "displayName"
        <*> value .:? "email"
        <*> value .:? "emailTypes" .!= ["INTERNET"]

instance ToJSON CalendarSummary where
  toJSON calendar =
    object
      [ "id" .= calendarSummaryId calendar
      , "displayName" .= calendarSummaryDisplayName calendar
      , "color" .= calendarSummaryColor calendar
      , "timezone" .= calendarSummaryTimezone calendar
      ]

instance ToJSON CalendarEntrySummary where
  toJSON entry =
    object
      [ "id" .= calendarEntryId entry
      , "calendarId" .= calendarEntryCalendarId entry
      , "calendarName" .= calendarEntryCalendarName entry
      , "title" .= calendarEntryTitle entry
      , "startsAt" .= calendarEntryStartsAt entry
      , "endsAt" .= calendarEntryEndsAt entry
      , "location" .= calendarEntryLocation entry
      , "description" .= calendarEntryDescription entry
      , "allDay" .= calendarEntryAllDay entry
      ]

instance ToJSON TodoSummary where
  toJSON todo =
    object
      [ "id" .= todoSummaryId todo
      , "calendarId" .= todoSummaryCalendarId todo
      , "title" .= todoSummaryTitle todo
      , "status" .= todoSummaryStatus todo
      , "due" .= todoSummaryDue todo
      ]
