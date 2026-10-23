{-# LANGUAGE OverloadedStrings #-}

module Mates.Server.ClientProfile
  ( ClientProfile (..)
  , clientProfileName
  , recognizeClient
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Text (Text)
import Network.HTTP.Types.Header (Header, hUserAgent)

data ClientProfile
  = AppleContacts
  | AppleCalendar
  | AppleDAV
  | GenericDAV
  deriving (Eq, Show)

recognizeClient :: [Header] -> ClientProfile
recognizeClient headers =
  case fmap normalizeUserAgent (lookup hUserAgent headers) of
    Just userAgent
      | containsAny ["addressbook", "contacts"] userAgent
          && isApple userAgent ->
          AppleContacts
      | containsAny ["calendaragent", "ical", "caldav"] userAgent
          && isApple userAgent ->
          AppleCalendar
      | isApple userAgent -> AppleDAV
    _ -> GenericDAV

clientProfileName :: ClientProfile -> Text
clientProfileName AppleContacts = "apple-contacts"
clientProfileName AppleCalendar = "apple-calendar"
clientProfileName AppleDAV = "apple-dav"
clientProfileName GenericDAV = "generic-dav"

normalizeUserAgent :: ByteString -> String
normalizeUserAgent =
  map toLower . ByteString.unpack

isApple :: String -> Bool
isApple userAgent =
  containsAny ["apple", "cfnetwork", "dataaccessd", "mac os x"] userAgent

containsAny :: [String] -> String -> Bool
containsAny needles haystack =
  any (`isInfixOf` haystack) needles
