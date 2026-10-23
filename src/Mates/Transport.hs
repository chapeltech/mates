{-# LANGUAGE ForeignFunctionInterface #-}
module Mates.Transport (request) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Foreign
import Foreign.C

foreign import ccall safe "mates_http" http :: CString -> CString -> CString -> CString -> CString -> CString -> CSize -> Ptr CString -> Ptr CSize -> Ptr CLong -> CString -> IO CInt

request :: String -> String -> String -> String -> String -> String -> IO (Either String (Int, String))
request url user password method depth body =
  withUtf8 url $ \u -> withUtf8 user $ \n -> withUtf8 password $ \p ->
  withUtf8 method $ \m -> withUtf8 depth $ \d ->
  BS.useAsCStringLen (TE.encodeUtf8 (T.pack body)) $ \(b,len) ->
  alloca $ \output -> alloca $ \outputLength -> alloca $ \status -> allocaBytes 256 $ \err -> do
    poke output nullPtr; poke outputLength 0; pokeByteOff err 0 (0 :: Word8)
    code <- http u n p m d b (fromIntegral len) output outputLength status err
    buffer <- peek output
    bracket (pure buffer) free $ \ptr ->
      if code /= 0 then Left <$> peekCString err else do
        size <- peek outputLength
        bytes <- if ptr == nullPtr then pure BS.empty else BS.packCStringLen (ptr, fromIntegral size)
        responseStatus <- peek status
        pure (Right (fromIntegral responseStatus, T.unpack (TE.decodeUtf8With lenientDecode bytes)))
  where withUtf8 value = BS.useAsCString (TE.encodeUtf8 (T.pack value))
