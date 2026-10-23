module Mates.Digest (sha256, etag) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)

sha256 :: BS.ByteString -> Text
sha256 = T.pack . concatMap hex . BS.unpack . SHA256.hash
  where hex x = let s = showHex x "" in replicate (2 - length s) '0' ++ s

etag :: LBS.ByteString -> Text
etag body = T.concat [T.singleton '"', sha256 (LBS.toStrict body), T.singleton '"']
