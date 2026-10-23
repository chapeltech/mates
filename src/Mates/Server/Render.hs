{-# LANGUAGE OverloadedStrings #-}

module Mates.Server.Render
  ( RenderedPayload (..)
  , renderCalendarObject
  , renderContactVCard
  , renderContactVCardFromRaw
  , renderDavPayload
  )
where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString as ByteString
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Mates.Server.ClientProfile (ClientProfile (..))
import Mates.VCard (VCardProperty (..), unfoldLines)
import Mates.Server.Types
  ( ContactEmail (..)
  , ContactRecord (..)
  , ContactRelationship (..)
  , DavPayload (..)
  )

data RenderedPayload = RenderedPayload
  { renderedContentType :: Text
  , renderedBody :: LazyByteString.ByteString
  }
  deriving (Eq, Show)

renderDavPayload :: ClientProfile -> DavPayload -> RenderedPayload
renderDavPayload profile payload =
  case payload of
    DavContact contact ->
      RenderedPayload
        { renderedContentType = "text/vcard; charset=utf-8"
        , renderedBody = encodeBody (renderContactVCard profile contact)
        }
    DavCalendarObject body ->
      RenderedPayload
        { renderedContentType = "text/calendar; charset=utf-8"
        , renderedBody = encodeBody (renderCalendarObject profile body)
        }

renderContactVCard :: ClientProfile -> ContactRecord -> Text
renderContactVCard profile contact =
  vcardLines
    ( [ "BEGIN:VCARD"
      , "VERSION:4.0"
      , "UID:" <> escapeVCardText (contactRecordId contact)
      , "FN:" <> escapeVCardText (contactRecordDisplayName contact)
      ]
        ++ map renderEmail (contactRecordEmails contact)
        ++ renderRelationships profile (contactRecordRelationships contact)
        ++ ["END:VCARD"]
    )

renderContactVCardFromRaw :: ClientProfile -> Text -> ContactRecord -> Text
renderContactVCardFromRaw profile rawBody contact =
  let originalLines = map Text.pack (unfoldLines (lines (Text.unpack rawBody)))
      projected = contactRecordProperties contact
      rawLines = if not (null projected) && length projected == length (filter (not . Text.null . Text.strip) originalLines) then map renderProperty projected else originalLines
      relatedGroups = appleRelationshipGroups rawLines
      keptLines = filter (not . isRelationshipWireLine relatedGroups) rawLines
      relationshipLines = renderRelationships profile (contactRecordRelationships contact)
      uncertainRelationships = any (\p -> propertyName p `elem` ["RELATED","X-ABRELATEDNAMES"] && any (\(key,value) -> key `notElem` ["TYPE","VALUE"] || key == "TYPE" && ',' `elem` value) (propertyParams p)) projected
   in if uncertainRelationships then rawBody else vcardLines (insertBeforeEndVCard relationshipLines (map (Text.dropWhileEnd (== '\r')) keptLines))

renderCalendarObject :: ClientProfile -> Text -> Text
renderCalendarObject _ body =
  body

renderEmail :: ContactEmail -> Text
renderEmail email =
  "EMAIL"
    <> renderTypeParameters (contactEmailTypes email)
    <> ":"
    <> escapeVCardText (contactEmailAddress email)

renderRelationships :: ClientProfile -> [ContactRelationship] -> [Text]
renderRelationships profile relationships =
  case profile of
    AppleContacts -> renderAppleRelationships relationships
    AppleDAV -> renderAppleRelationships relationships
    _ -> map renderGenericRelationship relationships

renderAppleRelationships :: [ContactRelationship] -> [Text]
renderAppleRelationships =
  concat . zipWith renderAppleRelationship [1 :: Int ..]

renderAppleRelationship :: Int -> ContactRelationship -> [Text]
renderAppleRelationship index relationship =
  [ groupName <> ".X-ABRELATEDNAMES:"
      <> escapeVCardText (relationshipName relationship)
  , groupName <> ".X-ABLabel:_$!<"
      <> escapeVCardText (relationshipLabel relationship)
      <> ">!$_"
  ] ++ maybe [] (\uri -> [groupName <> ".RELATED;VALUE=uri;TYPE=" <> escapeVCardParameter (relationshipLabel relationship) <> ":" <> uri]) (relationshipTarget relationship)
  where
    groupName = "item" <> Text.pack (show index)

renderGenericRelationship :: ContactRelationship -> Text
renderGenericRelationship relationship =
  "RELATED;TYPE=" <> escapeVCardParameter (relationshipLabel relationship)
    <> maybe ";VALUE=text:" (const ";VALUE=uri:") (relationshipTarget relationship)
    <> maybe (escapeVCardText (relationshipName relationship)) id (relationshipTarget relationship)

insertBeforeEndVCard :: [Text] -> [Text] -> [Text]
insertBeforeEndVCard inserted linesIn =
  let (beforeEnd, afterEnd) =
        break ((== "END:VCARD") . Text.toUpper . Text.strip) linesIn
   in case afterEnd of
        [] -> linesIn ++ inserted
        endLine : rest -> beforeEnd ++ inserted ++ (endLine : rest)

appleRelationshipGroups :: [Text] -> [Text]
appleRelationshipGroups =
  uniqueTexts . mapMaybe relatedGroup
  where
    relatedGroup line =
      case vcardPropertyName line of
        (Just groupName, "X-ABRELATEDNAMES") -> Just groupName
        _ -> Nothing

isRelationshipWireLine :: [Text] -> Text -> Bool
isRelationshipWireLine appleGroups line =
  case vcardPropertyName line of
    (_, "RELATED") -> True
    (_, "X-ABRELATEDNAMES") -> True
    (Just groupName, "X-ABLABEL") -> groupName `elem` appleGroups
    _ -> False

vcardPropertyName :: Text -> (Maybe Text, Text)
vcardPropertyName line =
  let left = fst (Text.breakOn ":" line)
      namePart = fst (Text.breakOn ";" left)
   in case Text.splitOn "." namePart of
        [groupName, localName] ->
          (Just groupName, Text.toUpper localName)
        _ ->
          (Nothing, Text.toUpper namePart)

uniqueTexts :: [Text] -> [Text]
uniqueTexts =
  foldl addIfMissing []
  where
    addIfMissing seen value
      | value `elem` seen = seen
      | otherwise = seen <> [value]

renderTypeParameters :: [Text] -> Text
renderTypeParameters [] = ""
renderTypeParameters types =
  ";TYPE=" <> Text.intercalate "," (map escapeVCardParameter types)

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

encodeBody :: Text -> LazyByteString.ByteString
encodeBody =
  LazyByteString.fromStrict . Text.encodeUtf8

vcardLines :: [Text] -> Text
vcardLines values = Text.intercalate "\r\n" (concatMap (foldLine 75 . Text.dropWhileEnd (== '\r')) values) <> "\r\n"
  where
    foldLine limit value
      | Text.null value = [""]
      | otherwise = let (prefix,suffix) = takeBytes limit value in prefix : if Text.null suffix then [] else map (" " <>) (foldLine 74 suffix)
    takeBytes limit value = go 0 Text.empty value
      where
        go count prefix remaining = case Text.uncons remaining of
          Nothing -> (prefix, Text.empty)
          Just (char,rest) -> let size = ByteString.length (Text.encodeUtf8 (Text.singleton char)) in if count + size > limit then (prefix,remaining) else go (count + size) (Text.snoc prefix char) rest

renderProperty :: VCardProperty -> Text
renderProperty property =
  maybe "" ((<> ".") . Text.pack) (propertyGroup property)
    <> Text.pack (propertyName property)
    <> Text.concat [";" <> Text.pack key <> "=" <> Text.pack value | (key,value) <- propertyParams property]
    <> ":" <> Text.pack (propertyRawValue property)
