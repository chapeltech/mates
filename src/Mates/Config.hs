module Mates.Config
  ( Config (..)
  , configPath
  , parseConfig
  , readConfig
  , renderConfig
  , writeConfig
  )
where

import Data.Char (isSpace)
import Data.List (find)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getXdgDirectory
  , XdgDirectory (XdgConfig)
  )
import System.FilePath (takeDirectory)

data Config = Config
  { configServer :: String
  , configUser :: String
  , configAddressBook :: Maybe String
  , configAuthzId :: Maybe String
  }
  deriving (Eq, Show)

configPath :: IO FilePath
configPath = getXdgDirectory XdgConfig "mates/config"

writeConfig :: Config -> IO FilePath
writeConfig config = do
  path <- configPath
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path (renderConfig config)
  pure path

readConfig :: IO (Maybe Config)
readConfig = do
  path <- configPath
  exists <- doesFileExist path
  if exists
    then do
      parsed <- parseConfig <$> readFile path
      case parsed of
        Right config -> pure (Just config)
        Left _ -> pure Nothing
    else pure Nothing

renderConfig :: Config -> String
renderConfig config =
  unlines
    ( [ "server=" ++ configServer config
      , "user=" ++ configUser config
      ]
        ++ addressBookLine
        ++ authzIdLine
    )
  where
    addressBookLine =
      case configAddressBook config of
        Just value | not (null value) -> ["addressbook=" ++ value]
        _ -> []
    authzIdLine =
      case configAuthzId config of
        Just value | not (null value) -> ["authzid=" ++ value]
        _ -> []

parseConfig :: String -> Either String Config
parseConfig input = do
  pairs <- traverse parseLine meaningfulLines
  server <- requireKey "server" pairs
  user <- requireKey "user" pairs
  let addressBook = lookupKey "addressbook" pairs
      authzId =
        firstJust
          [ lookupKey "authzid" pairs
          , lookupKey "negotiate-authzid" pairs
          , lookupKey "gssapi-authzid" pairs
          ]
  pure
    Config
      { configServer = server
      , configUser = user
      , configAddressBook = addressBook
      , configAuthzId = nonEmpty authzId
      }
  where
    meaningfulLines =
      filter (not . isIgnoredLine) (lines input)

parseLine :: String -> Either String (String, String)
parseLine line =
  case break (== '=') line of
    (key, '=' : value)
      | not (null (trim key)) -> Right (trim key, trim value)
    _ -> Left ("invalid config line: " ++ line)

requireKey :: String -> [(String, String)] -> Either String String
requireKey key pairs =
  case lookupKey key pairs of
    Just value | not (null value) -> Right value
    _ -> Left ("missing required config key: " ++ key)

lookupKey :: String -> [(String, String)] -> Maybe String
lookupKey key =
  fmap snd . find ((== key) . fst)

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just value : _) = Just value
firstJust (Nothing : rest) = firstJust rest

nonEmpty :: Maybe String -> Maybe String
nonEmpty (Just value)
  | not (null value) = Just value
nonEmpty _ = Nothing

isIgnoredLine :: String -> Bool
isIgnoredLine line =
  let stripped = trim line
   in null stripped || take 1 stripped == "#"

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate = reverse . dropWhile predicate . reverse
