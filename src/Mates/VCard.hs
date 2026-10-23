module Mates.VCard
  ( VCardProperty (..)
  , parseProperties
  , splitQuoted
  , VCard (..)
  , VCardEmail (..)
  , VCardField (..)
  , VCardPhoto (..)
  , cardEmailAddresses
  , cardFields
  , cardPhoto
  , decodeText
  , displayName
  , formatSummary
  , matchesVCardQuery
  , parseVCards
  , propertyValues
  , renderHumanReadable
  , unfoldLines
  , vcardId
  )
where

import Data.Char (isSpace, ord, toUpper)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)

data VCard = VCard
  { cardRaw :: String
  , cardUid :: Maybe String
  , cardFormattedName :: Maybe String
  , cardEmails :: [String]
  , cardPhones :: [String]
  }
  deriving (Eq, Show)

data VCardEmail = VCardEmail
  { vcardEmailAddress :: String
  , vcardEmailTypes :: [String]
  }
  deriving (Eq, Show)

data VCardField = VCardField
  { vcardFieldName :: String
  , vcardFieldLabel :: Maybe String
  , vcardFieldValue :: String
  }
  deriving (Eq, Show)

data VCardPhoto = VCardPhoto
  { vcardPhotoContentType :: String
  , vcardPhotoSource :: String
  }
  deriving (Eq, Show)

data VCardProperty = VCardProperty
  { propertyGroup :: Maybe String
  , propertyName :: String
  , propertyParams :: [(String, String)]
  , propertyRawValue :: String
  }
  deriving (Eq, Show)

parseVCards :: String -> Either String [VCard]
parseVCards input =
  case collectCards (unfoldLines (map stripTrailingCarriageReturn (lines input))) of
    [] -> Left "no BEGIN:VCARD/END:VCARD blocks found"
    cards -> traverse parseCard cards

formatSummary :: VCard -> String
formatSummary card =
  intercalate "  " (name : contact)
  where
    name = displayName card
    contact =
      case cardEmails card of
        email : _ -> [email]
        [] ->
          case cardPhones card of
            phone : _ -> [phone]
            [] -> []

matchesVCardQuery :: String -> VCard -> Bool
matchesVCardQuery query card =
  let needle = normalize query
   in null needle
        || any (needle `isInfixOf`) (map normalize (searchFields card))

cardEmailAddresses :: VCard -> [VCardEmail]
cardEmailAddresses card =
  [ VCardEmail
      { vcardEmailAddress = address
      , vcardEmailTypes = map (map toUpper) (typeLabels prop)
      }
  | prop <- propertiesNamed "EMAIL" (parseProperties card)
  , let address = propertyTextValue prop
  , not (null address)
  ]

cardFields :: VCard -> [VCardField]
cardFields card =
  filter
    (not . null . vcardFieldValue)
    ( simpleFields
        ++ map fieldFromEmail (propertiesNamed "EMAIL" props)
        ++ map fieldFromPhone (propertiesNamed "TEL" props)
        ++ map fieldFromAddress (propertiesNamed "ADR" props)
        ++ map fieldFromUrl (propertiesNamed "URL" props)
        ++ map fieldFromRelated relatedNames
        ++ imageFields
        ++ map fieldFromNote (propertiesNamed "NOTE" props)
    )
  where
    props = parseProperties card
    simpleFields =
      concat
        [ simpleField "Nickname" (firstPropertyValue "NICKNAME" props)
        , simpleField "Organization" (firstStructuredPropertyValue "ORG" props)
        , simpleField "Title" (firstPropertyValue "TITLE" props)
        , simpleField "Role" (firstPropertyValue "ROLE" props)
        , simpleField "Birthday" (firstPropertyValue "BDAY" props)
        , simpleField "Death date" (firstPropertyValue "DEATHDATE" props)
        , simpleField "Anniversary" (firstPropertyValue "ANNIVERSARY" props)
        , simpleField "Birth place" (firstPropertyValue "BIRTHPLACE" props)
        , simpleField "Death place" (firstPropertyValue "DEATHPLACE" props)
        ]
    relatedNames = relatedNameProperties props
    imageFields =
      if any isImageProperty props then
        [ VCardField
            { vcardFieldName = "Image"
            , vcardFieldLabel = Nothing
            , vcardFieldValue = "present (not displayed)"
            }
        ]

      else
        []

cardPhoto :: VCard -> Maybe VCardPhoto
cardPhoto card =
  case mapMaybe photoFromProperty imageProps of
    photo : _ -> Just photo
    [] -> Nothing
  where
    props = parseProperties card
    imageProps = propertiesNamed "PHOTO" props ++ propertiesNamed "LOGO" props

renderHumanReadable :: VCard -> String
renderHumanReadable card =
  unlines
    ( [displayName card]
        ++ simpleFields
        ++ section "Email" (map formatLabeledValue (propertiesNamed "EMAIL" props))
        ++ section "Phone" (map formatLabeledValue (propertiesNamed "TEL" props))
        ++ section "Address" (map formatStructuredLabeledValue (propertiesNamed "ADR" props))
        ++ section "URL" (map formatPlainValue (propertiesNamed "URL" props))
        ++ section "Related" (map formatRelatedName relatedNames)
        ++ imageLines
        ++ section "Note" (concatMap formatNote (propertiesNamed "NOTE" props))
    )
  where
    props = parseProperties card
    simpleFields =
      concat
        [ field "Nickname" (firstPropertyValue "NICKNAME" props)
        , field "Organization" (firstStructuredPropertyValue "ORG" props)
        , field "Title" (firstPropertyValue "TITLE" props)
        , field "Role" (firstPropertyValue "ROLE" props)
        , field "Birthday" (firstPropertyValue "BDAY" props)
        , field "Death date" (firstPropertyValue "DEATHDATE" props)
        , field "Anniversary" (firstPropertyValue "ANNIVERSARY" props)
        , field "Birth place" (firstPropertyValue "BIRTHPLACE" props)
        , field "Death place" (firstPropertyValue "DEATHPLACE" props)
        ]
    relatedNames = relatedNameProperties props
    imageLines =
      if any isImageProperty props
        then ["Image: present (not displayed)"]
        else []

displayName :: VCard -> String
displayName card =
  maybe "(no name)" id (cardFormattedName card)

searchFields :: VCard -> [String]
searchFields card =
  [ displayName card
  , vcardId card
  ]
    ++ maybeToList (cardUid card)
    ++ maybeToList (cardFormattedName card)
    ++ cardEmails card
    ++ cardPhones card

vcardId :: VCard -> String
vcardId card =
  case cardUid card of
    Just uid | not (null uid) -> uid
    _ -> "local-" ++ show (positiveHash (cardRaw card))

propertyValues :: String -> [String] -> [String]
propertyValues wantedName =
  map decodeText . foldr collect []
  where
    collect line values =
      case break (== ':') line of
        (property, ':' : value)
          | canonicalPropertyName property == canonicalPropertyName wantedName ->
              trim value : values
        _ -> values

unfoldLines :: [String] -> [String]
unfoldLines =
  foldr appendFoldedLine []
  where
    appendFoldedLine line (next : rest)
      | isContinuation next = (line ++ drop 1 next) : rest
    appendFoldedLine line rest = line : rest

decodeText :: String -> String
decodeText [] = []
decodeText ('\\' : next : rest) =
  decodeEscaped next : decodeText rest
decodeText (char : rest) = char : decodeText rest

parseCard :: [String] -> Either String VCard
parseCard cardLines =
  if hasBegin && hasEnd
    then
      Right
        VCard
          { cardRaw = unlines cardLines
          , cardUid = firstValue "UID"
          , cardFormattedName = firstValue "FN"
          , cardEmails = propertyValues "EMAIL" cardLines
          , cardPhones = propertyValues "TEL" cardLines
          }
    else Left "invalid vCard block"
  where
    hasBegin = any ((== "BEGIN:VCARD") . map toUpper) cardLines
    hasEnd = any ((== "END:VCARD") . map toUpper) cardLines
    firstValue name =
      case propertyValues name cardLines of
        value : _ -> Just value
        [] -> Nothing

collectCards :: [String] -> [[String]]
collectCards =
  goOutside []
  where
    goOutside cards [] = reverse cards
    goOutside cards (line : rest)
      | isLine "BEGIN:VCARD" line = goInside cards [line] rest
      | otherwise = goOutside cards rest

    goInside cards current [] = reverse (reverse current : cards)
    goInside cards current (line : rest)
      | isLine "END:VCARD" line = goOutside (reverse (line : current) : cards) rest
      | otherwise = goInside cards (line : current) rest

canonicalPropertyName :: String -> String
canonicalPropertyName =
  map toUpper . takeWhile (/= ';') . trim

decodeEscaped :: Char -> Char
decodeEscaped 'n' = '\n'
decodeEscaped 'N' = '\n'
decodeEscaped '\\' = '\\'
decodeEscaped ';' = ';'
decodeEscaped ',' = ','
decodeEscaped char = char

isContinuation :: String -> Bool
isContinuation (' ' : _) = True
isContinuation ('\t' : _) = True
isContinuation _ = False

isLine :: String -> String -> Bool
isLine expected line = map toUpper line == expected

positiveHash :: String -> Integer
positiveHash =
  foldl' step 2166136261
  where
    step hash char = (hash * 16777619 + toInteger (ord char)) `mod` 2147483647

stripTrailingCarriageReturn :: String -> String
stripTrailingCarriageReturn line =
  case reverse line of
    '\r' : rest -> reverse rest
    _ -> line

normalize :: String -> String
normalize = map toLowerAscii

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate = reverse . dropWhile predicate . reverse

parseProperties :: VCard -> [VCardProperty]
parseProperties =
  mapMaybe parsePropertyLine . unfoldLines . lines . cardRaw

parsePropertyLine :: String -> Maybe VCardProperty
parsePropertyLine line =
  case break (== ':') line of
    (left, ':' : value)
      | not (null parsedName) ->
          Just
            VCardProperty
              { propertyGroup = parsedGroup
              , propertyName = map toUpper parsedName
              , propertyParams = parsedParams
              , propertyRawValue = dropWhileEnd (== '\r') value
              }
      where
        (namePart, paramParts) =
          case splitQuoted ';' left of
            [] -> ("", [])
            part : parts -> (part, parts)
        (parsedGroup, parsedName) =
          parseGroupedName namePart
        parsedParams = map parseParam paramParts
    _ -> Nothing

parseGroupedName :: String -> (Maybe String, String)
parseGroupedName name =
  case break (== '.') name of
    (groupName, '.' : rest) -> (Just groupName, rest)
    _ -> (Nothing, name)

parseParam :: String -> (String, String)
parseParam param =
  case break (== '=') param of
    (key, '=' : value) -> (map toUpper (trim key), trim value)
    _ -> ("TYPE", trim param)

propertiesNamed :: String -> [VCardProperty] -> [VCardProperty]
propertiesNamed name =
  filter ((== map toUpper name) . propertyName)

firstPropertyValue :: String -> [VCardProperty] -> Maybe String
firstPropertyValue name props =
  case propertiesNamed name props of
    prop : _ -> Just (propertyTextValue prop)
    [] -> Nothing

firstStructuredPropertyValue :: String -> [VCardProperty] -> Maybe String
firstStructuredPropertyValue name props =
  case propertiesNamed name props of
    prop : _ -> Just (structuredPropertyText prop)
    [] -> Nothing

propertyTextValue :: VCardProperty -> String
propertyTextValue =
  trim . decodeText . propertyRawValue

structuredPropertyText :: VCardProperty -> String
structuredPropertyText =
  intercalate ", " . filter (not . null) . map trim . splitOn ';' . propertyTextValue

formatPlainValue :: VCardProperty -> String
formatPlainValue prop = "  " ++ propertyTextValue prop

formatLabeledValue :: VCardProperty -> String
formatLabeledValue prop =
  formatMaybeLabel (typeLabel prop) (propertyTextValue prop)

formatStructuredLabeledValue :: VCardProperty -> String
formatStructuredLabeledValue prop =
  formatMaybeLabel (typeLabel prop) (structuredPropertyText prop)

formatMaybeLabel :: Maybe String -> String -> String
formatMaybeLabel maybeLabel value =
  case maybeLabel of
    Just label | not (null label) -> "  " ++ label ++ ": " ++ value
    _ -> "  " ++ value

typeLabel :: VCardProperty -> Maybe String
typeLabel prop =
  case typeLabels prop of
    [] -> Nothing
    labels -> Just (intercalate ", " labels)

typeLabels :: VCardProperty -> [String]
typeLabels prop =
  filter (not . isIgnoredType) (concatMap paramTypes (propertyParams prop))

paramTypes :: (String, String) -> [String]
paramTypes ("TYPE", value) =
  map cleanTypeLabel (splitOn ',' value)
paramTypes _ = []

cleanTypeLabel :: String -> String
cleanTypeLabel =
  map toLowerAscii . trim

toLowerAscii :: Char -> Char
toLowerAscii char
  | char >= 'A' && char <= 'Z' = toEnum (ord char + 32)
  | otherwise = char

isIgnoredType :: String -> Bool
isIgnoredType value =
  value == "pref" || null value

field :: String -> Maybe String -> [String]
field label (Just value)
  | not (null value) = [label ++ ": " ++ value]
field _ _ = []

simpleField :: String -> Maybe String -> [VCardField]
simpleField name (Just value)
  | not (null value) =
      [ VCardField
          { vcardFieldName = name
          , vcardFieldLabel = Nothing
          , vcardFieldValue = value
          }
      ]
simpleField _ _ = []

fieldFromEmail :: VCardProperty -> VCardField
fieldFromEmail prop =
  VCardField
    { vcardFieldName = "Email"
    , vcardFieldLabel = typeLabel prop
    , vcardFieldValue = propertyTextValue prop
    }

fieldFromPhone :: VCardProperty -> VCardField
fieldFromPhone prop =
  VCardField
    { vcardFieldName = "Phone"
    , vcardFieldLabel = typeLabel prop
    , vcardFieldValue = propertyTextValue prop
    }

fieldFromAddress :: VCardProperty -> VCardField
fieldFromAddress prop =
  VCardField
    { vcardFieldName = "Address"
    , vcardFieldLabel = typeLabel prop
    , vcardFieldValue = structuredPropertyText prop
    }

fieldFromUrl :: VCardProperty -> VCardField
fieldFromUrl prop =
  VCardField
    { vcardFieldName = "URL"
    , vcardFieldLabel = typeLabel prop
    , vcardFieldValue = propertyTextValue prop
    }

fieldFromRelated :: (String, String) -> VCardField
fieldFromRelated (label, name) =
  VCardField
    { vcardFieldName = "Related"
    , vcardFieldLabel = Just label
    , vcardFieldValue = name
    }

fieldFromNote :: VCardProperty -> VCardField
fieldFromNote prop =
  VCardField
    { vcardFieldName = "Note"
    , vcardFieldLabel = Nothing
    , vcardFieldValue = propertyTextValue prop
    }

section :: String -> [String] -> [String]
section _ [] = []
section label values = (label ++ ":") : values

formatNote :: VCardProperty -> [String]
formatNote =
  map ("  " ++) . lines . propertyTextValue

relatedNameProperties :: [VCardProperty] -> [(String, String)]
relatedNameProperties props =
  [ (relatedLabel prop props, propertyTextValue prop)
  | prop <- propertiesNamed "X-ABRELATEDNAMES" props ++ propertiesNamed "RELATED" props
  , not (null (propertyTextValue prop))
  , propertyName prop /= "X-ABRELATEDNAMES" || not (any (\other -> propertyName other == "RELATED" && propertyGroup prop /= Nothing && propertyGroup other == propertyGroup prop) props)
  ]

relatedLabel :: VCardProperty -> [VCardProperty] -> String
relatedLabel prop props =
  case propertyGroup prop of
    Just groupName ->
      case [ cleanAppleLabel (propertyTextValue labelProp)
           | labelProp <- propertiesNamed "X-ABLABEL" props
           , propertyGroup labelProp == Just groupName
           ] of
        label : _ | not (null label) -> label
        _ -> fallbackRelatedLabel prop
    Nothing -> fallbackRelatedLabel prop

fallbackRelatedLabel :: VCardProperty -> String
fallbackRelatedLabel prop =
  case typeLabel prop of
    Just label | not (null label) -> label
    _ -> "related"

cleanAppleLabel :: String -> String
cleanAppleLabel value
  | "_$!<" `isPrefixOf` value && ">!$_" `isSuffixOf` value =
      take (length value - 8) (drop 4 value)
  | otherwise = value

formatRelatedName :: (String, String) -> String
formatRelatedName (label, name) =
  "  " ++ label ++ ": " ++ name

isImageProperty :: VCardProperty -> Bool
isImageProperty prop =
  propertyName prop == "PHOTO" || propertyName prop == "LOGO"

photoFromProperty :: VCardProperty -> Maybe VCardPhoto
photoFromProperty prop
  | isDataImageUri decodedValue =
      Just
        VCardPhoto
          { vcardPhotoContentType = photoContentTypeFromDataUri decodedValue
          , vcardPhotoSource = decodedValue
          }
  | isWebUri decodedValue =
      Just
        VCardPhoto
          { vcardPhotoContentType = photoMimeType prop
          , vcardPhotoSource = decodedValue
          }
  | isEmbeddedPhoto prop rawValue =
      Just
        VCardPhoto
          { vcardPhotoContentType = contentType
          , vcardPhotoSource =
              "data:" ++ contentType ++ ";base64," ++ compactBase64 rawValue
          }
  | otherwise = Nothing
  where
    decodedValue = propertyTextValue prop
    rawValue = propertyRawValue prop
    contentType = photoMimeType prop

isEmbeddedPhoto :: VCardProperty -> String -> Bool
isEmbeddedPhoto prop value =
  not (null (compactBase64 value))
    && (hasEncoding "b" prop || hasEncoding "base64" prop)

hasEncoding :: String -> VCardProperty -> Bool
hasEncoding wanted prop =
  any
    (\value -> map toLowerAscii value == wanted)
    (paramValues "ENCODING" prop)

photoMimeType :: VCardProperty -> String
photoMimeType prop =
  case paramValues "MEDIATYPE" prop of
    value : _ | isImageMime value -> map toLowerAscii value
    _ ->
      case mapMaybe photoTypeMime (typeLabels prop) of
        contentType : _ -> contentType
        [] -> "image/jpeg"

photoTypeMime :: String -> Maybe String
photoTypeMime value
  | isImageMime value = Just (map toLowerAscii value)
  | lower == "jpeg" = Just "image/jpeg"
  | lower == "jpg" = Just "image/jpeg"
  | lower == "png" = Just "image/png"
  | lower == "gif" = Just "image/gif"
  | lower == "webp" = Just "image/webp"
  | lower == "bmp" = Just "image/bmp"
  | lower == "tiff" = Just "image/tiff"
  | otherwise = Nothing
  where
    lower = map toLowerAscii value

photoContentTypeFromDataUri :: String -> String
photoContentTypeFromDataUri value =
  case break (\char -> char == ';' || char == ',') (drop 5 value) of
    (contentType, _)
      | isImageMime contentType -> map toLowerAscii contentType
    _ -> "image/jpeg"

isImageMime :: String -> Bool
isImageMime =
  startsWithIgnoreCase "image/"

isDataImageUri :: String -> Bool
isDataImageUri =
  startsWithIgnoreCase "data:image/"

isWebUri :: String -> Bool
isWebUri value =
  startsWithIgnoreCase "https://" value || startsWithIgnoreCase "http://" value

startsWithIgnoreCase :: String -> String -> Bool
startsWithIgnoreCase prefix value =
  map toLowerAscii prefix `isPrefixOf` map toLowerAscii value

compactBase64 :: String -> String
compactBase64 =
  filter (not . isSpace)

paramValues :: String -> VCardProperty -> [String]
paramValues wanted prop =
  [ value
  | (key, value) <- propertyParams prop
  , key == map toUpper wanted
  ]

splitOn :: Char -> String -> [String]
splitOn delimiter =
  go []
  where
    go current [] = [reverse current]
    go current (char : rest)
      | char == delimiter = reverse current : go [] rest
      | otherwise = go (char : current) rest

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (value : values) =
  case f value of
    Just result -> result : mapMaybe f values
    Nothing -> mapMaybe f values

maybeToList :: Maybe a -> [a]
maybeToList (Just value) = [value]
maybeToList Nothing = []

-- Parameter separators inside quoted strings belong to the parameter value.
splitQuoted :: Char -> String -> [String]
splitQuoted delimiter = go False False [] []
  where
    go _ _ current result [] = reverse (reverse current : result)
    go quoted escaped current result (c:rest)
      | escaped = go quoted False (c:current) result rest
      | c == '\\' = go quoted True (c:current) result rest
      | c == '"' = go (not quoted) False (c:current) result rest
      | c == delimiter && not quoted = go False False [] (reverse current:result) rest
      | otherwise = go quoted False (c:current) result rest
