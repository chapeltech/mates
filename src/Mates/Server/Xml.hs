{-# LANGUAGE OverloadedStrings #-}
module Mates.Server.Xml
  ( davResponses, parseXml, name, named, children, descendants, contents, attribute, dav, card, cal, xmlEscape ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.XML

parseXml :: LBS.ByteString -> Either Text Element
parseXml body
  | "<!DOCTYPE" `T.isInfixOf` TE.decodeUtf8With lenientDecode (LBS.toStrict body) || "<!ENTITY" `T.isInfixOf` TE.decodeUtf8With lenientDecode (LBS.toStrict body) = Left "DTD and entity declarations are unsupported"
  | otherwise = case parseLBS def body of
      Left err -> Left (T.pack (show err))
      Right doc -> Right (documentRoot doc)

name :: Text -> Text -> Name
name namespace local = Name local (Just namespace) Nothing
named :: Text -> Text -> Element -> Bool
named namespace local element = elementName element == name namespace local
children :: Element -> [Element]
children e = [child | NodeElement child <- elementNodes e]
descendants :: Element -> [Element]
descendants e = e : concatMap descendants (children e)
contents :: Element -> Text
contents e = T.concat [case n of NodeContent value -> value; NodeElement child -> contents child; _ -> "" | n <- elementNodes e]
attribute :: Text -> Element -> Maybe Text
attribute key e = Map.lookup (Name key Nothing Nothing) (elementAttributes e)
dav, card, cal :: Text
dav = "DAV:"
card = "urn:ietf:params:xml:ns:carddav"
cal = "urn:ietf:params:xml:ns:caldav"
xmlEscape :: Text -> Text
xmlEscape = T.concatMap (\c -> case c of '&' -> "&amp;"; '<' -> "&lt;"; '>' -> "&gt;"; '"' -> "&quot;"; '\'' -> "&apos;"; _ -> T.singleton c)

davResponses :: LBS.ByteString -> Either Text [(Text,[Element])]
davResponses body = do
  root <- parseXml body
  if not (named dav "multistatus" root) then Left "Expected DAV:multistatus" else
    Right [(contents href, successful response) | response <- children root, named dav "response" response, href <- take 1 (filter (named dav "href") (children response))]
  where
    successful response = [property | propstat <- children response, named dav "propstat" propstat, any (\s -> named dav "status" s && " 200 " `T.isInfixOf` contents s) (children propstat), prop <- children propstat, named dav "prop" prop, property <- children prop]
