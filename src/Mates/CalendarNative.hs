{-# LANGUAGE ForeignFunctionInterface #-}
module Mates.CalendarNative (timezoneCalendar, normalizeCalendar, occurrences, CalendarFailure (..)) where

import Control.Exception (Exception, bracket, throw)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall safe "mates_calendar_timezone" timezone :: CString -> IO CString
foreign import ccall safe "mates_calendar_normalize" normalize :: CString -> IO CString
foreign import ccall safe "mates_calendar_occurrences" expand :: CString -> CString -> CString -> Int64 -> Int64 -> IO CString

data CalendarFailure = CalendarFailure String deriving (Show)
instance Exception CalendarFailure

-- These wrappers expose deterministic, mutex-protected parsing operations.
-- Neither operation performs I/O or changes the stored calendar body.
{-# NOINLINE normalizeCalendar #-}
normalizeCalendar :: Text -> Either Text Text
normalizeCalendar body = unsafePerformIO $ withText body $ \input -> do
  ptr <- normalize input
  if ptr == nullPtr then pure (Left (T.pack "Invalid calendar or unresolved timezone")) else bracket (pure ptr) free (fmap (Right . TE.decodeUtf8) . BS.packCString)

{-# NOINLINE occurrences #-}
occurrences :: Text -> Text -> Text -> UTCTime -> UTCTime -> [UTCTime]
occurrences body uid kind start end = unsafePerformIO $
  withText body $ \input -> withText uid $ \u -> withText kind $ \k -> do
    ptr <- expand input u k (floor (utcTimeToPOSIXSeconds start)) (ceiling (utcTimeToPOSIXSeconds end))
    if ptr == nullPtr then throw (CalendarFailure "Calendar recurrence expansion exceeded its limit or is invalid") else
      bracket (pure ptr) free $ \result -> do
        value <- peekCString result
        pure [posixSecondsToUTCTime (fromInteger n) | line <- lines value, [(n,"")] <- [reads line]]

withText :: Text -> (CString -> IO a) -> IO a
withText value = BS.useAsCString (TE.encodeUtf8 value)

{-# NOINLINE timezoneCalendar #-}
timezoneCalendar :: Text -> Maybe Text
timezoneCalendar zone = unsafePerformIO $ withText zone $ \input -> do
  ptr <- timezone input
  if ptr == nullPtr then pure Nothing else bracket (pure ptr) free (fmap (Just . TE.decodeUtf8) . BS.packCString)
