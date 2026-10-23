module Mates.Cache
  ( CachedCard (..)
  , cacheFileForId
  , cacheRoot
  , cardsDirectory
  , ensureCache
  , findCard
  , findCards
  , loadCards
  , readCachedResponse
  , readFreshCachedResponse
  , searchCards
  , storeCard
  , writeCachedResponse
  )
where

import Data.Char (isAlphaNum, ord, toLower)
import Data.List (find, isInfixOf, isSuffixOf)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Mates.VCard (VCard (..), matchesVCardQuery, parseVCards, vcardId)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getModificationTime
  , getXdgDirectory
  , listDirectory
  , XdgDirectory (XdgCache)
  )
import System.FilePath ((</>), takeBaseName)

data CachedCard = CachedCard
  { cachedId :: String
  , cachedPath :: FilePath
  , cachedCard :: VCard
  }
  deriving (Eq, Show)

cacheRoot :: IO FilePath
cacheRoot = getXdgDirectory XdgCache "mates"

cardsDirectory :: IO FilePath
cardsDirectory = (</> "cards") <$> cacheRoot

responsesDirectory :: IO FilePath
responsesDirectory = (</> "responses") <$> cacheRoot

ensureCache :: IO FilePath
ensureCache = do
  directory <- cardsDirectory
  createDirectoryIfMissing True directory
  pure directory

cacheFileForId :: String -> IO FilePath
cacheFileForId identifier = do
  directory <- cardsDirectory
  pure (directory </> (safeFileName identifier ++ ".vcf"))

storeCard :: VCard -> IO CachedCard
storeCard card = do
  _ <- ensureCache
  path <- cacheFileForId identifier
  writeFile path (cardRaw card)
  pure CachedCard {cachedId = identifier, cachedPath = path, cachedCard = card}
  where
    identifier = vcardId card

loadCards :: IO [CachedCard]
loadCards = do
  directory <- cardsDirectory
  exists <- doesDirectoryExist directory
  if exists
    then do
      names <- listDirectory directory
      cards <- traverse (loadCachedFile directory) (filter isVCardFile names)
      pure (foldr keepJust [] cards)
    else pure []

findCard :: String -> IO (Maybe CachedCard)
findCard identifier =
  find (matchesIdentifier identifier) <$> loadCards

findCards :: String -> IO [CachedCard]
findCards query = do
  cards <- loadCards
  pure (filter (matchesCachedCardQuery query) cards)

readFreshCachedResponse :: NominalDiffTime -> String -> IO (Maybe String)
readFreshCachedResponse maxAge cacheKey = do
  path <- responseCacheFile cacheKey
  exists <- doesFileExist path
  if exists
    then do
      modified <- getModificationTime path
      now <- getCurrentTime
      if diffUTCTime now modified <= maxAge
        then Just <$> readFile path
        else pure Nothing
    else pure Nothing

readCachedResponse :: String -> IO (Maybe String)
readCachedResponse cacheKey = do
  path <- responseCacheFile cacheKey
  exists <- doesFileExist path
  if exists
    then Just <$> readFile path
    else pure Nothing

writeCachedResponse :: String -> String -> IO ()
writeCachedResponse cacheKey response = do
  directory <- responsesDirectory
  createDirectoryIfMissing True directory
  path <- responseCacheFile cacheKey
  writeFile path response

searchCards :: String -> IO [CachedCard]
searchCards = findCards

loadCachedFile :: FilePath -> FilePath -> IO (Maybe CachedCard)
loadCachedFile directory name = do
  let path = directory </> name
  raw <- readFile path
  pure
    ( case parseVCards raw of
        Right (card : _) ->
          Just
            CachedCard
              { cachedId = vcardId card
              , cachedPath = path
              , cachedCard = card
              }
        _ -> Nothing
    )

matchesIdentifier :: String -> CachedCard -> Bool
matchesIdentifier identifier cached =
  identifier == cachedId cached
    || identifier == takeBaseName (cachedPath cached)
    || Just identifier == cardUid (cachedCard cached)

matchesCachedCardQuery :: String -> CachedCard -> Bool
matchesCachedCardQuery query cached =
  let needle = normalize query
   in null needle
        || any
          (needle `isInfixOf`)
          (map normalize [cachedId cached, takeBaseName (cachedPath cached)])
        || matchesVCardQuery query (cachedCard cached)

isVCardFile :: FilePath -> Bool
isVCardFile = isSuffixOf ".vcf"

safeFileName :: String -> String
safeFileName identifier =
  case take 128 (map safeChar identifier) of
    [] -> "card"
    name -> name

safeChar :: Char -> Char
safeChar char
  | isAlphaNum char = char
  | char `elem` ("._-" :: String) = char
  | otherwise = '-'

normalize :: String -> String
normalize = map toLower

keepJust :: Maybe a -> [a] -> [a]
keepJust (Just value) values = value : values
keepJust Nothing values = values

responseCacheFile :: String -> IO FilePath
responseCacheFile cacheKey = do
  directory <- responsesDirectory
  pure (directory </> (show (positiveHash cacheKey) ++ ".body"))

positiveHash :: String -> Integer
positiveHash =
  foldl' step 2166136261
  where
    step hash char = (hash * 16777619 + toInteger (ord char)) `mod` 2147483647
