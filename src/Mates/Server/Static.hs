{-# LANGUAGE OverloadedStrings #-}

module Mates.Server.Static
  ( frontendApplication
  )
where

import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai
  ( Application
  , pathInfo
  , responseFile
  , responseLBS
  )
import System.Directory (doesFileExist)
import System.FilePath ((</>))

frontendApplication :: Application
frontendApplication request respond =
  case pathInfo request of
    [] -> serveFile "index.html" "text/html; charset=utf-8"
    [""] -> serveFile "index.html" "text/html; charset=utf-8"
    ["ui"] -> serveFile "index.html" "text/html; charset=utf-8"
    ["ui", ""] -> serveFile "index.html" "text/html; charset=utf-8"
    ["index.html"] -> serveFile "index.html" "text/html; charset=utf-8"
    ["ui", "index.html"] -> serveFile "index.html" "text/html; charset=utf-8"
    ["styles.css"] -> serveFile "styles.css" "text/css; charset=utf-8"
    ["ui", "styles.css"] -> serveFile "styles.css" "text/css; charset=utf-8"
    ["main.js"] -> serveFile "main.js" "application/javascript; charset=utf-8"
    ["ui", "main.js"] -> serveFile "main.js" "application/javascript; charset=utf-8"
    _ -> respond notFound
 where
  serveFile file contentType = do
    let path = "frontend" </> file
    exists <- doesFileExist path
    if exists
      then respond (responseFile status200 [(hContentType, contentType)] path Nothing)
      else respond notFound

  notFound =
    responseLBS
      status404
      [(hContentType, "text/plain; charset=utf-8")]
      "not found\n"
