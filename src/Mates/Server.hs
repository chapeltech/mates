{-# LANGUAGE OverloadedStrings #-}
module Mates.Server
  ( ServerConfig (..)
  , defaultServerConfig
  , runServer
  )
where

import qualified Mates.Server.API as API
import Mates.Server.Security (SecurityConfig (..), AuthMode (..), validPublicUrl)
import Data.String (fromString)
import Data.Text (Text)
import Mates.Server.Store (newPostgresStoreFromEnvironmentWithOrigin)
import qualified Network.Wai.Handler.Warp as Warp

data ServerConfig = ServerConfig
  { serverHost :: String
  , serverAuth :: AuthMode
  , serverPublicUrl :: Text
  , serverPort :: Int
  }
  deriving (Eq, Show)

defaultServerConfig :: ServerConfig
defaultServerConfig =
  ServerConfig {serverPort = 8080, serverHost = "127.0.0.1", serverAuth = ProxyAuth, serverPublicUrl = ""}

runServer :: ServerConfig -> IO ()
runServer config = do
  if not (validPublicUrl (serverPublicUrl config)) then ioError (userError "serve requires --public-url http(s)://HOST (an origin without credentials or a path)") else pure ()
  if serverAuth config == NoAuth && serverHost config `notElem` ["127.0.0.1", "::1", "localhost"] then ioError (userError "--auth none requires a loopback host") else pure ()
  store <- newPostgresStoreFromEnvironmentWithOrigin (serverPublicUrl config)
  Warp.runSettings (Warp.setHost (fromString (serverHost config)) (Warp.setPort (serverPort config) Warp.defaultSettings))
    (API.applicationWithSecurity (SecurityConfig (serverAuth config) (serverPublicUrl config)) store)
