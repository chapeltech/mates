module Mates.App
  ( mainWith
  , usage
  )
where

import qualified Mates.Cache as Cache
import qualified Mates.CardDAV as CardDAV
import qualified Mates.Config as Config
import qualified Mates.Server as Server
import Mates.Server.Security (AuthMode (..), validPublicUrl)
import Mates.Server.Store (Store (..), newPostgresStoreFromEnvironment)
import qualified Data.Text as T
import Mates.VCard
  ( VCard (cardRaw)
  , VCardEmail (..)
  , cardEmailAddresses
  , displayName
  , parseVCards
  , renderHumanReadable
  )
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

mainWith :: [String] -> IO ()
mainWith args =
  case args of
    [] -> putStr usage
    ["--help"] -> putStr usage
    ["-h"] -> putStr usage
    "init" : rest -> initCommand rest
    "import" : rest -> importCommand rest
    "email" : "--remote" : rest -> remoteEmailCommand rest
    "email" : rest -> emailCommand rest
    "list" : "--remote" : rest -> remoteListCommand rest
    "list" : rest -> listCommand rest
    "remote" : "email" : rest -> remoteEmailCommand rest
    "remote" : "list" : rest -> remoteListCommand rest
    "serve" : rest -> serveCommand rest
    "principal" : rest -> adminCommand ("principal" : rest)
    "collection" : rest -> adminCommand ("collection" : rest)
    "access" : rest -> adminCommand ("access" : rest)
    ["show", "--remote", identifier] -> remoteShowCommand identifier
    ["show", identifier] -> showCommand identifier
    ["export", identifier] -> exportCommand identifier Nothing
    ["export", identifier, path] -> exportCommand identifier (Just path)
    ["sync"] -> syncCommand
    command : _ -> failWith ("unknown command: " ++ command)

usage :: String
usage =
  unlines
    [ "mates - command line vCard, iCalendar, and DAV manager"
    , ""
    , "Usage:"
    , "  mates init --server URL --user USER [--addressbook PATH] [--authzid AUTHZID]"
    , "  mates import FILE [FILE ...]"
    , "  mates email [QUERY]"
    , "  mates email --remote [QUERY]"
    , "  mates list [QUERY]"
    , "  mates list --remote [QUERY]"
    , "  mates remote email [QUERY]"
    , "  mates remote list [QUERY]"
    , "  mates serve [--port PORT] [--host HOST] [--auth proxy|none] [--public-url URL]"
    , "  mates principal add|disable USER; principal list"
    , "  mates collection list; collection create /dav/calendars/NAME/ DISPLAY_NAME"
    , "  mates collection timezone COLLECTION IANA_ZONE"
    , "  mates access list; access grant USER COLLECTION read|write; access revoke USER COLLECTION"
    , "  mates show NAME|ID"
    , "  mates show --remote NAME"
    , "  mates export ID [FILE]"
    , ""
    , "Remote reads are cached automatically and reused when fresh or offline."
    ]

initCommand :: [String] -> IO ()
initCommand args =
  case parseOptions args of
    Left message -> failWith message
    Right options -> do
      server <- requireOption ["server"] options
      user <- requireOption ["user", "username"] options
      let addressBook = lookupOption "addressbook" options
          authzId =
            lookupAnyOption
              ["authzid", "negotiate-authzid", "gssapi-authzid"]
              options
      path <-
        Config.writeConfig
          Config.Config
            { Config.configServer = server
            , Config.configUser = user
            , Config.configAddressBook = addressBook
            , Config.configAuthzId = authzId
            }
      cachePath <- Cache.ensureCache
      putStrLn ("Wrote config: " ++ path)
      putStrLn ("Prepared cache: " ++ cachePath)

importCommand :: [String] -> IO ()
importCommand [] = failWith "import requires at least one vCard file"
importCommand paths = do
  imported <- traverse importFile paths
  putStrLn ("Imported " ++ show (sum imported) ++ " card(s).")

listCommand :: [String] -> IO ()
listCommand args = do
  cards <- Cache.searchCards (unwords args)
  if null cards
    then putStrLn "No cached cards found."
    else mapM_ printCachedSummary cards

remoteListCommand :: [String] -> IO ()
remoteListCommand args = do
  config <- Config.readConfig
  case config of
    Nothing -> failWith "run `mates init` before listing remote cards"
    Just loadedConfig -> do
      result <- CardDAV.listRemoteCards loadedConfig (unwords args)
      case result of
        Right cards ->
          if null cards
            then putStrLn "No remote cards found."
            else mapM_ printRemoteSummary cards
        Left message -> failWith message

emailCommand :: [String] -> IO ()
emailCommand args = do
  cards <- Cache.searchCards (unwords args)
  printEmailResults "No cached email addresses found." (map Cache.cachedCard cards)

remoteEmailCommand :: [String] -> IO ()
remoteEmailCommand args = do
  config <- Config.readConfig
  case config of
    Nothing -> failWith "run `mates init` before listing remote email addresses"
    Just loadedConfig -> do
      result <- CardDAV.listRemoteCards loadedConfig (unwords args)
      case result of
        Right cards ->
          printEmailResults
            "No remote email addresses found."
            (map CardDAV.remoteCard cards)
        Left message -> failWith message

showCommand :: String -> IO ()
showCommand identifier = do
  found <- Cache.findCard identifier
  case found of
    Just cached -> putStr (renderHumanReadable (Cache.cachedCard cached))
    Nothing -> do
      matches <- Cache.findCards identifier
      case matches of
        [cached] -> putStr (renderHumanReadable (Cache.cachedCard cached))
        [] -> showRemoteFallback identifier
        ambiguous ->
          failWith
            ( "multiple cached cards found for "
                ++ show identifier
                ++ ": "
                ++ joinNames (map (displayName . Cache.cachedCard) ambiguous)
            )

showRemoteFallback :: String -> IO ()
showRemoteFallback identifier = do
  config <- Config.readConfig
  case config of
    Nothing -> failWith ("no cached card found for: " ++ identifier)
    Just loadedConfig -> showRemoteWithConfig loadedConfig identifier

remoteShowCommand :: String -> IO ()
remoteShowCommand identifier = do
  config <- Config.readConfig
  case config of
    Nothing -> failWith "run `mates init` before showing remote cards"
    Just loadedConfig -> showRemoteWithConfig loadedConfig identifier

showRemoteWithConfig :: Config.Config -> String -> IO ()
showRemoteWithConfig config identifier = do
  result <- CardDAV.showRemoteCard config identifier
  case result of
    Right raw -> putStr (renderRawCards raw)
    Left message -> failWith message

exportCommand :: String -> Maybe FilePath -> IO ()
exportCommand identifier outputPath = do
  found <- Cache.findCard identifier
  case found of
    Just cached ->
      case outputPath of
        Just path -> writeFile path (cachedRaw cached)
        Nothing -> putStr (cachedRaw cached)
    Nothing -> failWith ("no cached card found for ID: " ++ identifier)

syncCommand :: IO ()
syncCommand =
  failWith
    ( "sync is no longer a separate command; "
        ++ "remote list/show cache results automatically"
    )

serveCommand :: [String] -> IO ()
serveCommand args =
  case parseOptions args of
    Left message -> failWith message
    Right options -> do
      port <- parsePort (lookupOption "port" options)
      mode <- case lookupOption "auth" options of
        Nothing -> pure ProxyAuth
        Just "proxy" -> pure ProxyAuth
        Just "none" -> pure NoAuth
        _ -> failWith "--auth must be proxy or none"
      let host = maybe "127.0.0.1" id (lookupOption "host" options)
          origin = maybe (if mode == NoAuth then "http://" ++ (if host == "::1" then "[::1]" else host) ++ ":" ++ show port else "") id (lookupOption "public-url" options)
      if not (validPublicUrl (T.pack origin)) then failWith "serve requires --public-url http(s)://HOST (an origin without credentials or a path)" else pure ()
      if mode == NoAuth && host `notElem` ["127.0.0.1", "::1", "localhost"] then failWith "--auth none requires a loopback --host" else pure ()
      putStrLn ("Serving mates on http://" ++ host ++ ":" ++ show port)
      Server.runServer Server.defaultServerConfig {Server.serverPort = port, Server.serverHost = host, Server.serverAuth = mode, Server.serverPublicUrl = T.dropWhileEnd (== '/') (T.pack origin)}

adminCommand :: [String] -> IO ()
adminCommand args = do
  store <- newPostgresStoreFromEnvironment
  result <- storeAdmin store (map T.pack args)
  case result of
    Left problem -> failWith (T.unpack problem)
    Right rows -> mapM_ (putStrLn . T.unpack) rows

importFile :: FilePath -> IO Int
importFile path = do
  raw <- readFile path
  case parseVCards raw of
    Left message -> failWith (path ++ ": " ++ message)
    Right cards -> do
      _ <- traverse Cache.storeCard cards
      pure (length cards)

printCachedSummary :: Cache.CachedCard -> IO ()
printCachedSummary cached =
  putStrLn (displayName (Cache.cachedCard cached))

printRemoteSummary :: CardDAV.RemoteCard -> IO ()
printRemoteSummary remote =
  putStrLn (displayName (CardDAV.remoteCard remote))

printEmailResults :: String -> [VCard] -> IO ()
printEmailResults emptyMessage cards =
  case concatMap emailRows cards of
    [] -> putStrLn emptyMessage
    rows -> mapM_ putStrLn rows

emailRows :: VCard -> [String]
emailRows card =
  [ vcardEmailAddress email
      ++ "\t"
      ++ displayName card
      ++ "\t"
      ++ joinComma (vcardEmailTypes email)
  | email <- cardEmailAddresses card
  ]

parseOptions :: [String] -> Either String [(String, String)]
parseOptions [] = Right []
parseOptions (flag : value : rest)
  | Just name <- stripLongFlag flag = ((name, value) :) <$> parseOptions rest
parseOptions [flag]
  | Just _ <- stripLongFlag flag = Left ("missing value for option " ++ flag)
parseOptions (arg : _) = Left ("expected option, got " ++ arg)

stripLongFlag :: String -> Maybe String
stripLongFlag ('-' : '-' : name)
  | not (null name) = Just name
stripLongFlag _ = Nothing

requireOption :: [String] -> [(String, String)] -> IO String
requireOption names options =
  case foldr firstPresent Nothing names of
    Just value -> pure value
    Nothing -> failWith ("missing required option --" ++ primaryOptionName names)
  where
    firstPresent name found =
      case found of
        Just _ -> found
        Nothing -> lookupOption name options

primaryOptionName :: [String] -> String
primaryOptionName (name : _) = name
primaryOptionName [] = "unknown"

lookupOption :: String -> [(String, String)] -> Maybe String
lookupOption name =
  lookup name

lookupAnyOption :: [String] -> [(String, String)] -> Maybe String
lookupAnyOption names options =
  case names of
    [] -> Nothing
    name : rest ->
      case lookupOption name options of
        Just value -> Just value
        Nothing -> lookupAnyOption rest options

parsePort :: Maybe String -> IO Int
parsePort Nothing =
  pure (Server.serverPort Server.defaultServerConfig)
parsePort (Just value) =
  case readMaybe value of
    Just port | port > 0 && port < 65536 -> pure port
    _ -> failWith ("invalid port: " ++ value)

cachedRaw :: Cache.CachedCard -> String
cachedRaw = cardRaw . Cache.cachedCard

renderRawCards :: String -> String
renderRawCards raw =
  case parseVCards raw of
    Right cards -> concatMap renderHumanReadable cards
    Left _ -> raw

joinNames :: [String] -> String
joinNames [] = ""
joinNames [name] = name
joinNames (name : names) = name ++ ", " ++ joinNames names

joinComma :: [String] -> String
joinComma = joinNames

failWith :: String -> IO a
failWith message = do
  hPutStrLn stderr ("mates: " ++ message)
  hPutStrLn stderr "Run `mates --help` for usage."
  exitFailure
