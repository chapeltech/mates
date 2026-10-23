{-# LANGUAGE OverloadedStrings #-}
module Mates.CardDAV
  ( RemoteCard (..)
  , listRemoteCards
  , parseAddressBookHrefs
  , parseRemoteCards
  , showRemoteCard
  )
where

import Data.Char (isSpace, toLower)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified Mates.Server.Xml as X
import Data.Maybe (listToMaybe)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Time.Clock (NominalDiffTime)
import qualified Mates.Cache as Cache
import Mates.Config (Config (..))
import Mates.VCard (VCard (..), displayName, matchesVCardQuery, parseVCards, vcardId)
import System.Environment (lookupEnv)
import qualified Mates.Transport as Transport

data RemoteCard = RemoteCard
  { remoteHref :: String
  , remoteEtag :: Maybe String
  , remoteCard :: VCard
  , remoteAddressBookUrl :: String
  }
  deriving (Eq, Show)

listRemoteCards :: Config -> String -> IO (Either String [RemoteCard])
listRemoteCards config query = do
  cards <- listRemoteCardsUnfiltered config
  pure (filter (matchesRemoteCard config query) <$> cards)

showRemoteCard :: Config -> String -> IO (Either String String)
showRemoteCard config identifier = do
  cards <- listRemoteCardsUnfiltered config
  case matchingRemoteCards config identifier <$> cards of
    Left message -> pure (Left message)
    Right [remote] -> fetchRemoteCard config remote
    Right [] -> pure (Left ("no remote card found for: " ++ identifier))
    Right matches ->
      pure
        ( Left
            ( "multiple remote cards found for "
                ++ show identifier
                ++ ": "
                ++ joinNames (map (displayName . remoteCard) matches)
            )
        )

listRemoteCardsUnfiltered :: Config -> IO (Either String [RemoteCard])
listRemoteCardsUnfiltered config = do
  initial <- queryAddressBook config (addressBookUrl config)
  case initial of
    Right (_ : _) -> pure initial
    Right []
      | Nothing <- configAddressBook config -> queryDiscoveredAddressBooks config
      | otherwise -> pure initial
    Left message -> pure (Left message)

queryDiscoveredAddressBooks :: Config -> IO (Either String [RemoteCard])
queryDiscoveredAddressBooks config = do
  discovered <- discoverAddressBooks config
  case discovered of
    Left message -> pure (Left message)
    Right [] -> pure (Right [])
    Right urls -> collectRemoteCards config urls

collectRemoteCards :: Config -> [String] -> IO (Either String [RemoteCard])
collectRemoteCards config urls = do
  results <- traverse (queryAddressBook config) urls
  pure (combineResults results)

combineResults :: [Either String [RemoteCard]] -> Either String [RemoteCard]
combineResults results =
  let (cards, errors) = foldr collect ([], []) results
   in case (cards, errors) of
        (_ : _, _) -> Right cards
        ([], []) -> Right []
        ([], firstError : _) -> Left firstError
  where
    collect result (cards, errors) =
      case result of
        Right foundCards -> (foundCards ++ cards, errors)
        Left message -> (cards, message : errors)

queryAddressBook :: Config -> String -> IO (Either String [RemoteCard])
queryAddressBook config url = do
  report <- runCardDavRequest config url "REPORT" "1" addressBookQueryBody
  pure (map (withAddressBookUrl url) <$> (report >>= parseRemoteCards))

fetchRemoteCard :: Config -> RemoteCard -> IO (Either String String)
fetchRemoteCard config remote = do
  report <-
    runCardDavRequest
      config
      (remoteReportUrl config remote)
      "REPORT"
      "0"
      (addressBookMultigetBody (remoteHref remote))
  case report >>= parseSingleRemoteCard of
    Right raw -> do
      cacheFullVCardResponse raw
      pure (Right raw)
    Left _ ->
      pure (Right (cardRaw (remoteCard remote)))

discoverAddressBooks :: Config -> IO (Either String [String])
discoverAddressBooks config = do
  let configuredUrl = addressBookUrl config
  current <- propfindAddressBookHrefs config configuredUrl "0"
  case current of
    Right (_ : _) -> pure current
    Right [] -> do
      homes <- discoverAddressBookHomes config configuredUrl
      case homes of
        Right foundHomes@(_ : _) -> discoverHomeAddressBooks config foundHomes
        Right [] -> propfindAddressBookHrefs config configuredUrl "1"
        Left message -> pure (Left message)
    Left message -> pure (Left message)

discoverAddressBookHomes :: Config -> String -> IO (Either String [String])
discoverAddressBookHomes config url = do
  report <- runCardDavRequest config url "PROPFIND" "0" addressBookHomeSetBody
  case report of
    Left message -> pure (Left message)
    Right xml ->
      case map (absoluteUrl config) (parseAddressBookHomeSetHrefs xml) of
        [] -> discoverPrincipalAddressBookHomes config xml
        homes -> pure (Right homes)

discoverPrincipalAddressBookHomes :: Config -> String -> IO (Either String [String])
discoverPrincipalAddressBookHomes config xml = do
  results <-
    traverse
      (discoverAddressBookHomesDirect config . absoluteUrl config)
      (parseCurrentUserPrincipalHrefs xml)
  pure (dedupe <$> combineHrefResults results)

discoverAddressBookHomesDirect :: Config -> String -> IO (Either String [String])
discoverAddressBookHomesDirect config url = do
  report <- runCardDavRequest config url "PROPFIND" "0" addressBookHomeSetBody
  pure (map (absoluteUrl config) . parseAddressBookHomeSetHrefs <$> report)

discoverHomeAddressBooks :: Config -> [String] -> IO (Either String [String])
discoverHomeAddressBooks config homes = do
  results <- traverse (\url -> propfindAddressBookHrefs config url "1") homes
  pure (dedupe <$> combineHrefResults results)

propfindAddressBookHrefs :: Config -> String -> String -> IO (Either String [String])
propfindAddressBookHrefs config url depth = do
  report <- runCardDavRequest config url "PROPFIND" depth addressBookCollectionBody
  pure (map (absoluteUrl config) . parseAddressBookHrefs <$> report)

combineHrefResults :: [Either String [String]] -> Either String [String]
combineHrefResults results =
  let (hrefs, errors) = foldr collect ([], []) results
   in case (hrefs, errors) of
        (_ : _, _) -> Right hrefs
        ([], []) -> Right []
        ([], firstError : _) -> Left firstError
  where
    collect result (hrefs, errors) =
      case result of
        Right foundHrefs -> (foundHrefs ++ hrefs, errors)
        Left message -> (hrefs, message : errors)

runCardDavRequest :: Config -> String -> String -> String -> String -> IO (Either String String)
runCardDavRequest config url method depth body = do
  let cacheKey = requestCacheKey config url method depth body
  freshCached <- Cache.readFreshCachedResponse remoteCacheFreshSeconds cacheKey
  case freshCached of
    Just responseBody -> pure (Right responseBody)
    Nothing -> runCardDavRequestUncached config cacheKey url method depth body

remoteCacheFreshSeconds :: NominalDiffTime
remoteCacheFreshSeconds = 15

requestCacheKey :: Config -> String -> String -> String -> String -> String
requestCacheKey config url method depth body =
  unlines
    [ "carddav"
    , configServer config
    , configUser config
    , maybe "" id (configAuthzId config)
    , maybe "" id (configAddressBook config)
    , method
    , url
    , depth
    , body
    ]

runCardDavRequestUncached ::
  Config -> String -> String -> String -> String -> String -> IO (Either String String)
runCardDavRequestUncached config cacheKey url method depth body = do
  password <- lookupEnv "MATES_PASSWORD"
  result <- Transport.request url (curlUser config) (maybe "" id password) method depth body
  case result of
    Left message -> fallbackToCachedResponse cacheKey ("DAV transport failed: " ++ message)
    Right (status, responseBody)
      | status >= 200 && status < 300 -> cacheSuccessfulResponse cacheKey responseBody
      | otherwise -> fallbackToCachedResponse cacheKey ("CardDAV " ++ method ++ " failed with HTTP " ++ show status ++ responseDetail responseBody)

cacheSuccessfulResponse :: String -> String -> IO (Either String String)
cacheSuccessfulResponse cacheKey responseBody = do
  Cache.writeCachedResponse cacheKey responseBody
  pure (Right responseBody)

fallbackToCachedResponse :: String -> String -> IO (Either String String)
fallbackToCachedResponse cacheKey message = do
  cached <- Cache.readCachedResponse cacheKey
  case cached of
    Just responseBody -> pure (Right responseBody)
    Nothing -> pure (Left message)

cacheFullVCardResponse :: String -> IO ()
cacheFullVCardResponse raw =
  case parseVCards raw of
    Right cards -> mapM_ Cache.storeCard cards
    Left _ -> pure ()

curlUser :: Config -> String
curlUser config =
  case configAuthzId config of
    Just value | not (null value) -> value
    _ -> configUser config

addressBookUrl :: Config -> String
addressBookUrl config =
  ensureCollectionUrl
    ( case configAddressBook config of
        Just url | hasScheme url -> url
        Just path | "/" `isPrefixOf` path -> urlOrigin (configServer config) ++ path
        Just path -> joinUrlPath (configServer config) path
        Nothing -> configServer config
    )

addressBookQueryBody :: String
addressBookQueryBody =
  unlines
    [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    , "<C:addressbook-query"
    , "    xmlns:D=\"DAV:\""
    , "    xmlns:C=\"urn:ietf:params:xml:ns:carddav\">"
    , "  <D:prop>"
    , "    <D:getetag/>"
    , "    <C:address-data>"
    , "      <C:prop name=\"UID\"/>"
    , "      <C:prop name=\"FN\"/>"
    , "      <C:prop name=\"EMAIL\"/>"
    , "      <C:prop name=\"TEL\"/>"
    , "    </C:address-data>"
    , "  </D:prop>"
    , "  <C:filter>"
    , "    <C:prop-filter name=\"VERSION\"/>"
    , "  </C:filter>"
    , "</C:addressbook-query>"
    ]

addressBookMultigetBody :: String -> String
addressBookMultigetBody href =
  unlines
    [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    , "<C:addressbook-multiget"
    , "    xmlns:D=\"DAV:\""
    , "    xmlns:C=\"urn:ietf:params:xml:ns:carddav\">"
    , "  <D:prop>"
    , "    <D:getetag/>"
    , "    <C:address-data/>"
    , "  </D:prop>"
    , "  <D:href>" ++ T.unpack (X.xmlEscape (T.pack href)) ++ "</D:href>"
    , "</C:addressbook-multiget>"
    ]

addressBookHomeSetBody :: String
addressBookHomeSetBody =
  unlines
    [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    , "<D:propfind"
    , "    xmlns:D=\"DAV:\""
    , "    xmlns:C=\"urn:ietf:params:xml:ns:carddav\">"
    , "  <D:prop>"
    , "    <D:current-user-principal/>"
    , "    <C:addressbook-home-set/>"
    , "  </D:prop>"
    , "</D:propfind>"
    ]

addressBookCollectionBody :: String
addressBookCollectionBody =
  unlines
    [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    , "<D:propfind"
    , "    xmlns:D=\"DAV:\""
    , "    xmlns:C=\"urn:ietf:params:xml:ns:carddav\">"
    , "  <D:prop>"
    , "    <D:displayname/>"
    , "    <D:resourcetype/>"
    , "  </D:prop>"
    , "</D:propfind>"
    ]

parseRemoteCards :: String -> Either String [RemoteCard]
parseRemoteCards xml = do
  responses <- either (Left . T.unpack) Right (X.davResponses (LBS.fromStrict (TE.encodeUtf8 (T.pack xml))))
  pure [RemoteCard (T.unpack href) (T.unpack . X.contents <$> listToMaybe (filter (X.named X.dav "getetag") properties)) card "" | (href,properties) <- responses, addressData <- filter (X.named X.card "address-data") properties, Right cards <- [parseVCards (T.unpack (X.contents addressData))], card <- cards]

parseSingleRemoteCard :: String -> Either String String
parseSingleRemoteCard xml =
  case parseRemoteCards xml of
    Right [remote] -> Right (cardRaw (remoteCard remote))
    Right [] -> Left "CardDAV multiget response did not include address-data"
    Right cards ->
      Left
        ( "CardDAV multiget response returned "
            ++ show (length cards)
            ++ " cards"
        )
    Left message -> Left message

withAddressBookUrl :: String -> RemoteCard -> RemoteCard
withAddressBookUrl url remote =
  remote {remoteAddressBookUrl = url}

parseAddressBookHomeSetHrefs :: String -> [String]
parseAddressBookHomeSetHrefs = nestedHrefs X.card "addressbook-home-set"

parseCurrentUserPrincipalHrefs :: String -> [String]
parseCurrentUserPrincipalHrefs = nestedHrefs X.dav "current-user-principal"

nestedHrefs :: T.Text -> T.Text -> String -> [String]
nestedHrefs namespace local xml = case X.davResponses (LBS.fromStrict (TE.encodeUtf8 (T.pack xml))) of
  Left _ -> []
  Right responses -> dedupe [T.unpack (T.strip (X.contents h)) | (_,properties) <- responses, property <- properties, X.named namespace local property, h <- X.descendants property, X.named X.dav "href" h]

parseAddressBookHrefs :: String -> [String]
parseAddressBookHrefs xml = case X.davResponses (LBS.fromStrict (TE.encodeUtf8 (T.pack xml))) of
  Left _ -> []
  Right responses -> dedupe [T.unpack (T.strip href) | (href,properties) <- responses, any (\p -> X.named X.dav "resourcetype" p && any (X.named X.card "addressbook") (X.children p)) properties]

matchingRemoteCards :: Config -> String -> [RemoteCard] -> [RemoteCard]
matchingRemoteCards config identifier =
  filter (matchesRemoteCard config identifier)

matchesRemoteCard :: Config -> String -> RemoteCard -> Bool
matchesRemoteCard config identifier remote =
  let needle = normalize identifier
   in null needle
        || any
          (needle `isInfixOf`)
          ( map
              normalize
              [ remoteHref remote
              , absoluteResourceUrl config (remoteHref remote)
              ]
          )
        || identifier == vcardId card
        || Just identifier == cardUid card
        || matchesVCardQuery identifier card
  where
    card = remoteCard remote

responseDetail :: String -> String
responseDetail body =
  let bodyText = trim body
   in if null bodyText
        then ""
        else ": " ++ take 500 bodyText

hasScheme :: String -> Bool
hasScheme url =
  "http://" `isPrefixOf` url || "https://" `isPrefixOf` url

urlOrigin :: String -> String
urlOrigin url =
  case breakOn "://" url of
    Just (scheme, rest) -> scheme ++ "://" ++ takeWhile (/= '/') rest
    Nothing -> url

absoluteUrl :: Config -> String -> String
absoluteUrl config url
  | hasScheme url = ensureCollectionUrl url
  | "/" `isPrefixOf` url = ensureCollectionUrl (urlOrigin (configServer config) ++ url)
  | otherwise = ensureCollectionUrl (joinUrlPath (urlOrigin (configServer config)) url)

absoluteResourceUrl :: Config -> String -> String
absoluteResourceUrl config url
  | hasScheme url = url
  | "/" `isPrefixOf` url = urlOrigin (configServer config) ++ url
  | otherwise = joinUrlPath (urlOrigin (configServer config)) url

remoteReportUrl :: Config -> RemoteCard -> String
remoteReportUrl config remote
  | not (null (remoteAddressBookUrl remote)) = remoteAddressBookUrl remote
  | otherwise =
      ensureCollectionUrl (dropFileName (absoluteResourceUrl config (remoteHref remote)))

dropFileName :: String -> String
dropFileName url =
  case breakOnLastSlash url of
    Just collection -> collection
    Nothing -> url

breakOnLastSlash :: String -> Maybe String
breakOnLastSlash =
  fmap reverse . stripAfterSlash . reverse
  where
    stripAfterSlash [] = Nothing
    stripAfterSlash ('/' : rest) = Just ('/' : rest)
    stripAfterSlash (_ : rest) = stripAfterSlash rest

ensureCollectionUrl :: String -> String
ensureCollectionUrl url
  | "/" `isSuffixOf` url = url
  | otherwise = url ++ "/"

joinUrlPath :: String -> String -> String
joinUrlPath base path
  | "/" `isSuffixOf` base = base ++ path
  | otherwise = base ++ "/" ++ path

breakOn :: String -> String -> Maybe (String, String)
breakOn needle =
  go ""
  where
    go _ [] = Nothing
    go prefix remaining
      | needle `isPrefixOf` remaining = Just (prefix, drop (length needle) remaining)
      | otherwise =
          case remaining of
            char : rest -> go (prefix ++ [char]) rest

normalize :: String -> String
normalize = map toLower

joinNames :: [String] -> String
joinNames [] = ""
joinNames [name] = name
joinNames (name : names) = name ++ ", " ++ joinNames names

dedupe :: Eq a => [a] -> [a]
dedupe =
  foldr keep []
  where
    keep value values
      | value `elem` values = values
      | otherwise = value : values

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate = reverse . dropWhile predicate . reverse
