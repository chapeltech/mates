{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Mates.Server.API
  ( api
  , application
  , applicationWithSecurity
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import Mates.Server.DAV (davApplicationFor)
import Mates.Server.Security (SecurityConfig (..), AuthMode (..), authenticate)
import Network.HTTP.Types (mkStatus, status308)
import qualified Data.Text.Encoding as TE
import qualified Network.Wai as Wai
import Mates.Server.Static (frontendApplication)
import Mates.Server.Store (Store)
import Network.Wai (Application)
import Servant
  ( Get
  , Handler
  , JSON
  , Raw
  , Server
  , serve
  , type (:<|>) (..)
  , type (:>)
  )

data Health = Health
  { healthStatus :: Text
  , healthService :: Text
  }
  deriving (Eq, Show)

type API =
  "health" :> Get '[JSON] Health
    :<|> "ui" :> Raw
    :<|> "dav" :> Raw

api :: Proxy API
api = Proxy

application :: Store -> Application
application = applicationWithSecurity (SecurityConfig ProxyAuth "")

applicationWithSecurity :: SecurityConfig -> Store -> Application
applicationWithSecurity config store request respond
  | Wai.pathInfo request `elem` [[".well-known", "carddav"], [".well-known", "caldav"]] = respond (Wai.responseLBS status308 [("Location", TE.encodeUtf8 (publicUrl config <> "/dav/"))] "")
  | Wai.pathInfo request == ["health"] = serve api (server config Nothing store) request respond
  | otherwise = do
      identity <- authenticate config store request
      case identity of
        Left status -> respond (Wai.responseLBS (mkStatus status "Authentication required") [] "Authentication required")
        Right principal -> serve api (server config principal store) request respond

server :: SecurityConfig -> Maybe Text -> Store -> Server API
server config principal store =
  healthHandler
    :<|> Tagged frontendApplication
    :<|> Tagged (davApplicationFor principal (publicUrl config) store)

healthHandler :: Handler Health
healthHandler =
  pure Health {healthStatus = "ok", healthService = "mates"}

instance ToJSON Health where
  toJSON health =
    object
      [ "status" .= healthStatus health
      , "service" .= healthService health
      ]
