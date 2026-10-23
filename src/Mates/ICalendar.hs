{-# LANGUAGE OverloadedStrings #-}

module Mates.ICalendar
  ( ICalComponent (..)
  , ICalProperty (..)
  , parseCalendarTree
  , CalendarProjection (..)
  , CalendarRecurrenceDate (..)
  , CalendarTimeValue (..)
  , CalendarEventProjection (..)
  , CalendarTodoProjection (..)
  , calendarEntrySummaryFromBody
  , calendarObjectKindFromBody
  , calendarObjectMatchesTimeRange
  , calendarProjectionsFromBody
  , calendarTimeValueDateText
  , calendarTimeValueDateTimeText
  , recurrenceDateDateText
  , recurrenceDateDateTimeText
  , parseDurationSeconds
  )
where

import qualified Mates.CalendarNative as Native
import Data.Char (isDigit, toUpper)
import Control.Applicative ((<|>))
import Data.List (nub, sort)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar
  ( Day
  )
import Data.Time.Clock
  ( NominalDiffTime
  , UTCTime (..)
  , addUTCTime
  , diffUTCTime
  , nominalDay
  , secondsToNominalDiffTime
  )
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.LocalTime
  ( LocalTime
  , addLocalTime
  , localTimeToUTC
  , utc
  )
import Mates.Server.Types (CalendarEntrySummary (..))
import Mates.VCard (unfoldLines, splitQuoted)
import Text.Read (readMaybe)

data CalendarProjection = CalendarProjection
  { projectionSource :: Text
  , projectionOriginalProperties :: [(Text, [(Text,Text)], Text)]
  , projectionAlarms :: [[(Text, [(Text,Text)], Text)]]
  , projectionKind :: Text
  , projectionUid :: Text
  , projectionRecurrenceId :: Maybe Text
  , projectionSummary :: Maybe Text
  , projectionDescription :: Maybe Text
  , projectionLocation :: Maybe Text
  , projectionStatus :: Maybe Text
  , projectionPriority :: Maybe Int
  , projectionEvent :: Maybe CalendarEventProjection
  , projectionTodo :: Maybe CalendarTodoProjection
  , projectionRecurrenceRules :: [Text]
  , projectionRecurrenceDates :: [CalendarRecurrenceDate]
  }
  deriving (Eq, Show)

data CalendarEventProjection = CalendarEventProjection
  { eventStarts :: CalendarTimeValue
  , eventEnds :: Maybe CalendarTimeValue
  , eventDurationSeconds :: Maybe Int
  , eventIsAllDay :: Bool
  }
  deriving (Eq, Show)

data CalendarTodoProjection = CalendarTodoProjection
  { todoStarts :: Maybe CalendarTimeValue
  , todoDue :: Maybe CalendarTimeValue
  , todoCompleted :: Maybe CalendarTimeValue
  , todoCreated :: Maybe CalendarTimeValue
  , todoDurationSeconds :: Maybe Int
  , todoPercentComplete :: Maybe Int
  }
  deriving (Eq, Show)

data CalendarRecurrenceDate = CalendarRecurrenceDate
  { recurrenceDateKind :: Text
  , recurrenceDateValue :: CalendarTimeValue
  , recurrenceDateEnd :: Maybe CalendarTimeValue
  , recurrenceDateDurationSeconds :: Maybe Int
  }
  deriving (Eq, Show)

data CalendarTimeValue
  = CalendarDate Day
  | CalendarDateTime UTCTime
  | CalendarFloating LocalTime
  | CalendarZoned Text UTCTime
  deriving (Eq, Ord, Show)

data ICalComponent = ICalComponent
  { componentKind :: Text
  , componentProperties :: [ICalProperty]
  , componentChildren :: [ICalComponent]
  }
  deriving (Eq, Show)

data ICalProperty = ICalProperty
  { propertyName :: Text
  , propertyParameters :: [(Text, Text)]
  , propertyValue :: Text
  }
  deriving (Eq, Show)

data TimeRange = TimeRange
  { rangeStart :: Maybe UTCTime
  , rangeEnd :: Maybe UTCTime
  }
  deriving (Eq, Show)

calendarObjectKindFromBody :: Text -> Text
calendarObjectKindFromBody bodyText =
  case parseCalendarComponents bodyText of
    component : _ -> componentKind component
    [] ->
      if "BEGIN:VTODO" `Text.isInfixOf` bodyText then "VTODO" else "VEVENT"

calendarProjectionsFromBody :: Text -> Text -> Text -> [CalendarProjection]
calendarProjectionsFromBody calendarName fallbackUid bodyText =
  let normalized = either (const bodyText) id (Native.normalizeCalendar bodyText)
      originals = parseCalendarComponents bodyText
      components = parseCalendarComponents normalized
   in [projection { projectionSource = bodyText, projectionOriginalProperties = [(propertyName p, propertyParameters p, propertyValue p) | p <- componentProperties original], projectionAlarms = [[(propertyName p, propertyParameters p, propertyValue p) | p <- componentProperties alarm] | alarm <- componentChildren component, componentKind alarm == "VALARM"] } | (component,original) <- zip components originals, Just projection <- [calendarProjectionFromComponent calendarName fallbackUid component]]

calendarEntrySummaryFromBody :: Text -> Text -> Maybe CalendarEntrySummary
calendarEntrySummaryFromBody href bodyText =
  let calendarName = calendarNameFromHref href
      fallbackUid = calendarIdFromHref href
   in firstJust
        [ eventSummaryFromProjection href calendarName projection
        | projection <- calendarProjectionsFromBody calendarName fallbackUid bodyText
        ]

calendarObjectMatchesTimeRange :: Maybe Text -> Maybe Text -> Text -> Bool
calendarObjectMatchesTimeRange maybeStart maybeEnd bodyText =
  let timeRange =
        TimeRange
          { rangeStart = parseRangeTime =<< maybeStart
          , rangeEnd = parseRangeTime =<< maybeEnd
          }
      projections = calendarProjectionsFromBody "calendar" "calendar-object" bodyText
      overrideStarts =
        [ (projectionKind projection, projectionUid projection, timeValueInstant recurrenceId)
        | projection <- projections
        , Just recurrenceIdText <- [projectionRecurrenceId projection]
        , Just recurrenceId <- [parseCalendarTimeValue [] recurrenceIdText]
        ]
   in any (projectionMatchesTimeRange timeRange overrideStarts) projections

calendarTimeValueDateText :: CalendarTimeValue -> Maybe Text
calendarTimeValueDateText value =
  case value of
    CalendarDate day -> Just (formatDayText day)
    _ -> Nothing

calendarTimeValueDateTimeText :: CalendarTimeValue -> Maybe Text
calendarTimeValueDateTimeText value =
  case value of
    CalendarDate _ -> Nothing
    CalendarDateTime time -> Just (formatUtcText time)
    CalendarZoned _ time -> Just (formatUtcText time)
    CalendarFloating _ -> Nothing

recurrenceDateDateText :: CalendarRecurrenceDate -> Maybe Text
recurrenceDateDateText =
  calendarTimeValueDateText . recurrenceDateValue

recurrenceDateDateTimeText :: CalendarRecurrenceDate -> Maybe Text
recurrenceDateDateTimeText =
  calendarTimeValueDateTimeText . recurrenceDateValue

calendarProjectionFromComponent :: Text -> Text -> ICalComponent -> Maybe CalendarProjection
calendarProjectionFromComponent calendarName fallbackUid component =
  let properties = componentProperties component
      uid = fromMaybe fallbackUid (firstPropertyValue "UID" properties)
      recurrenceRules = propertyValue <$> propertiesNamed "RRULE" properties
      recurrenceDates =
        concatMap recurrenceDatesFromProperty (propertiesNamed "RDATE" properties)
          ++ concatMap recurrenceDatesFromProperty (propertiesNamed "EXDATE" properties)
      common eventProjection todoProjection =
        CalendarProjection
          { projectionSource = ""
          , projectionOriginalProperties = []
          , projectionAlarms = []
          , projectionKind = componentKind component
          , projectionUid = uid
          , projectionRecurrenceId = firstPropertyValue "RECURRENCE-ID" properties
          , projectionSummary = firstPropertyValue "SUMMARY" properties
          , projectionDescription = firstPropertyValue "DESCRIPTION" properties
          , projectionLocation =
              if componentKind component == "VEVENT" then firstPropertyValue "LOCATION" properties else Nothing
          , projectionStatus = firstPropertyValue "STATUS" properties
          , projectionPriority = parseIntText =<< firstPropertyValue "PRIORITY" properties
          , projectionEvent = eventProjection
          , projectionTodo = todoProjection
          , projectionRecurrenceRules = recurrenceRules
          , projectionRecurrenceDates = recurrenceDates
          }
   in case componentKind component of
        "VEVENT" -> do
          event <- eventProjectionFromProperties properties
          Just (common (Just event) Nothing)
        "VTODO" ->
          Just (common Nothing (Just (todoProjectionFromProperties properties)))
        _ ->
          let _ = calendarName
           in Nothing

eventProjectionFromProperties :: [ICalProperty] -> Maybe CalendarEventProjection
eventProjectionFromProperties properties = do
  startProperty <- firstProperty "DTSTART" properties
  start <- timeValueFromProperty startProperty
  let duration = parseDurationSeconds =<< firstPropertyValue "DURATION" properties
  Just
    CalendarEventProjection
      { eventStarts = start
      , eventEnds = firstTimeValue "DTEND" properties
      , eventDurationSeconds = duration
      , eventIsAllDay = isDateValue start
      }

todoProjectionFromProperties :: [ICalProperty] -> CalendarTodoProjection
todoProjectionFromProperties properties =
  CalendarTodoProjection
    { todoStarts = firstTimeValue "DTSTART" properties
    , todoDue = firstTimeValue "DUE" properties
    , todoCompleted = firstTimeValue "COMPLETED" properties
    , todoCreated = firstTimeValue "CREATED" properties
    , todoDurationSeconds = parseDurationSeconds =<< firstPropertyValue "DURATION" properties
    , todoPercentComplete = parseIntText =<< firstPropertyValue "PERCENT-COMPLETE" properties
    }

eventSummaryFromProjection :: Text -> Text -> CalendarProjection -> Maybe CalendarEntrySummary
eventSummaryFromProjection href calendarName projection =
  case projectionEvent projection of
    Just event ->
      Just
        CalendarEntrySummary
          { calendarEntryId = projectionUid projection
          , calendarEntryCalendarId = calendarName
          , calendarEntryCalendarName = calendarName
          , calendarEntryTitle = fromMaybe "(untitled event)" (projectionSummary projection)
          , calendarEntryStartsAt = calendarTimeValueText (eventStarts event)
          , calendarEntryEndsAt =
              calendarTimeValueText
                <$> (eventEnds event <|> eventDurationEnd event)
          , calendarEntryLocation = projectionLocation projection
          , calendarEntryDescription = projectionDescription projection
          , calendarEntryAllDay = eventIsAllDay event
          }
    Nothing ->
      let _ = href
       in Nothing

eventDurationEnd :: CalendarEventProjection -> Maybe CalendarTimeValue
eventDurationEnd event =
  case eventDurationSeconds event of
    Just seconds ->
      Just (shiftTimeValue (secondsToNominalDiffTime (fromIntegral seconds)) (eventStarts event))
    Nothing ->
      Nothing

projectionMatchesTimeRange ::
  TimeRange ->
  [(Text, Text, UTCTime)] ->
  CalendarProjection ->
  Bool
projectionMatchesTimeRange timeRange overrideStarts projection =
  case projectionEvent projection of
    Just event ->
      any
        (eventOccurrenceMatches timeRange projection event)
        (eventOccurrenceStarts timeRange overrideStarts projection event)
    Nothing ->
      case projectionTodo projection of
        Just todo ->
          any
            (todoOccurrenceMatches timeRange todo)
            (todoOccurrenceStarts timeRange overrideStarts projection todo)
        Nothing ->
          False

eventOccurrenceStarts ::
  TimeRange ->
  [(Text, Text, UTCTime)] ->
  CalendarProjection ->
  CalendarEventProjection ->
  [UTCTime]
eventOccurrenceStarts timeRange overrideStarts projection event =
  recurrenceStarts
    timeRange
    (projectionExclusions overrideStarts projection)
    (eventDurationForExpansion event)
    projection
    (timeValueInstant (eventStarts event))

todoOccurrenceStarts ::
  TimeRange ->
  [(Text, Text, UTCTime)] ->
  CalendarProjection ->
  CalendarTodoProjection ->
  [Maybe UTCTime]
todoOccurrenceStarts timeRange overrideStarts projection todo =
  case todoBaseTime todo of
    Just base ->
      Just
        <$> recurrenceStarts
          timeRange
          (projectionExclusions overrideStarts projection)
          (todoDurationForExpansion todo)
          projection
          (timeValueInstant base)
    Nothing ->
      [Nothing]

projectionExclusions ::
  [(Text, Text, UTCTime)] ->
  CalendarProjection ->
  [UTCTime]
projectionExclusions overrideStarts projection =
  let ownExDates =
        [ timeValueInstant (recurrenceDateValue date)
        | date <- projectionRecurrenceDates projection
        , recurrenceDateKind date == "EXDATE"
        ]
      overridden =
        [ occurrence
        | (kind, uid, occurrence) <- overrideStarts
        , kind == projectionKind projection
        , uid == projectionUid projection
        , projectionRecurrenceId projection == Nothing
        ]
   in ownExDates ++ overridden

eventOccurrenceMatches :: TimeRange -> CalendarProjection -> CalendarEventProjection -> UTCTime -> Bool
eventOccurrenceMatches timeRange projection event occurrenceStart =
  let baseStart = timeValueInstant (eventStarts event)
      delta = occurrenceStart `diffUTCTime` baseStart
      shiftedStart = occurrenceStart
      period = firstJust [Just date | date <- projectionRecurrenceDates projection, recurrenceDateKind date == "RDATE", timeValueInstant (recurrenceDateValue date) == occurrenceStart, recurrenceDateEnd date /= Nothing || recurrenceDateDurationSeconds date /= Nothing]
      periodEnd = period >>= (\date -> (timeValueInstant <$> recurrenceDateEnd date) <|> ((\seconds -> addUTCTime (fromIntegral seconds) occurrenceStart) <$> recurrenceDateDurationSeconds date))
      shiftedEnd = periodEnd <|> (shiftTimeValueInstant delta <$> eventEnds event)
      shiftedDuration = secondsToNominalDiffTime . fromIntegral <$> eventDurationSeconds event
   in case shiftedEnd of
        Just end ->
          rangeStartLt timeRange end && rangeEndGt timeRange shiftedStart
        Nothing ->
          case shiftedDuration of
            Just duration
              | duration > 0 ->
                  rangeStartLt timeRange (addUTCTime duration shiftedStart)
                    && rangeEndGt timeRange shiftedStart
              | otherwise ->
                  rangeStartLe timeRange shiftedStart
                    && rangeEndGt timeRange shiftedStart
            Nothing
              | eventIsAllDay event ->
                  rangeStartLt timeRange (addUTCTime nominalDay shiftedStart)
                    && rangeEndGt timeRange shiftedStart
              | otherwise ->
                  rangeStartLe timeRange shiftedStart
                    && rangeEndGt timeRange shiftedStart

todoOccurrenceMatches :: TimeRange -> CalendarTodoProjection -> Maybe UTCTime -> Bool
todoOccurrenceMatches timeRange todo maybeOccurrenceStart =
  let base = timeValueInstant <$> todoBaseTime todo
      delta = case (base, maybeOccurrenceStart) of
        (Just baseTime, Just occurrenceStart) -> occurrenceStart `diffUTCTime` baseTime
        _ -> 0
      shifted value = shiftTimeValueInstant delta <$> value
      dtstart = shifted (todoStarts todo)
      due = shifted (todoDue todo)
      completed = shifted (todoCompleted todo)
      created = shifted (todoCreated todo)
      duration = secondsToNominalDiffTime . fromIntegral <$> todoDurationSeconds todo
      effectiveDue = due <|> (addUTCTime <$> duration <*> dtstart)
   in case (dtstart, duration, due, completed, created) of
        (Just startTime, Just durationValue, Nothing, _, _) ->
          let endTime = addUTCTime durationValue startTime
           in rangeStartLe timeRange endTime
                && (rangeEndGt timeRange startTime || rangeEndGe timeRange endTime)
        (Just startTime, _, Just dueTime, _, _) ->
          (rangeStartLt timeRange dueTime || rangeStartLe timeRange startTime)
            && (rangeEndGt timeRange startTime || rangeEndGe timeRange dueTime)
        (Just startTime, _, Nothing, _, _) ->
          rangeStartLe timeRange startTime && rangeEndGt timeRange startTime
        (Nothing, _, Just dueTime, _, _) ->
          rangeStartLt timeRange dueTime && rangeEndGe timeRange dueTime
        (Nothing, _, Nothing, Just completedTime, Just createdTime) ->
          (rangeStartLe timeRange createdTime || rangeStartLe timeRange completedTime)
            && (rangeEndGe timeRange createdTime || rangeEndGe timeRange completedTime)
        (Nothing, _, Nothing, Just completedTime, Nothing) ->
          rangeStartLe timeRange completedTime && rangeEndGe timeRange completedTime
        (Nothing, _, Nothing, Nothing, Just createdTime) ->
          rangeEndGt timeRange createdTime
        (Nothing, Just _, Nothing, _, _) ->
          maybe True (\dueTime -> rangeEndGe timeRange dueTime) effectiveDue
        (Nothing, Nothing, Nothing, Nothing, Nothing) ->
          True

todoBaseTime :: CalendarTodoProjection -> Maybe CalendarTimeValue
todoBaseTime todo =
  todoStarts todo <|> todoDue todo <|> todoCreated todo <|> todoCompleted todo

eventDurationForExpansion :: CalendarEventProjection -> NominalDiffTime
eventDurationForExpansion event =
  case eventEnds event of
    Just end -> max 0 (timeValueInstant end `diffUTCTime` timeValueInstant (eventStarts event))
    Nothing ->
      case eventDurationSeconds event of
        Just seconds -> secondsToNominalDiffTime (fromIntegral (max 0 seconds))
        Nothing ->
          if eventIsAllDay event then nominalDay else 0

todoDurationForExpansion :: CalendarTodoProjection -> NominalDiffTime
todoDurationForExpansion todo =
  case (todoStarts todo, todoDue todo) of
    (Just startTime, Just dueTime) ->
      max 0 (timeValueInstant dueTime `diffUTCTime` timeValueInstant startTime)
    _ ->
      secondsToNominalDiffTime (fromIntegral (fromMaybe 0 (todoDurationSeconds todo)))

recurrenceStarts ::
  TimeRange ->
  [UTCTime] ->
  NominalDiffTime ->
  CalendarProjection ->
  UTCTime ->
  [UTCTime]
recurrenceStarts timeRange excluded durationPad projection base =
  let rdates = [timeValueInstant (recurrenceDateValue date) | date <- projectionRecurrenceDates projection, recurrenceDateKind date == "RDATE"]
      start = maybe base (addUTCTime (negate (max nominalDay durationPad))) (rangeStart timeRange)
      end = fromMaybe (addUTCTime (nominalDay * 366 * 200) (max start base)) (rangeEnd timeRange)
      expanded = if null (projectionRecurrenceRules projection) || projectionRecurrenceId projection /= Nothing then [] else Native.occurrences (projectionSource projection) (projectionUid projection) (projectionKind projection) start end
   in sort (nub [occurrence | occurrence <- base : rdates ++ expanded, occurrence `notElem` excluded])

parseIntText :: Text -> Maybe Int
parseIntText value = readMaybe (Text.unpack (Text.strip value))

recurrenceDatesFromProperty :: ICalProperty -> [CalendarRecurrenceDate]
recurrenceDatesFromProperty property =
  [ CalendarRecurrenceDate
      { recurrenceDateKind = propertyName property
      , recurrenceDateValue = value
      , recurrenceDateEnd = if Text.null afterSlash then Nothing else parseCalendarTimeValue (propertyParameters property) (Text.drop 1 afterSlash)
      , recurrenceDateDurationSeconds = if Text.null afterSlash then Nothing else parseDurationSeconds (Text.drop 1 afterSlash)
      }
  | rawValue <- Text.splitOn "," (propertyValue property)
  , let (startValue,afterSlash) = Text.breakOn "/" rawValue
  , Just value <- [parseCalendarTimeValue (propertyParameters property) startValue]
  ]

firstTimeValue :: Text -> [ICalProperty] -> Maybe CalendarTimeValue
firstTimeValue name properties =
  timeValueFromProperty =<< firstProperty name properties

timeValueFromProperty :: ICalProperty -> Maybe CalendarTimeValue
timeValueFromProperty property =
  parseCalendarTimeValue (propertyParameters property) (propertyValue property)

parseCalendarTimeValue :: [(Text, Text)] -> Text -> Maybe CalendarTimeValue
parseCalendarTimeValue parameters rawValue
  | hasParameterValue "VALUE" "DATE" parameters =
      CalendarDate <$> parseDayText rawValue
  | looksLikeDate rawValue =
      CalendarDate <$> parseDayText rawValue
  | Just tzid <- lookup "TZID" parameters = CalendarZoned tzid <$> parseRangeTime rawValue
  | "Z" `Text.isSuffixOf` rawValue = CalendarDateTime <$> parseRangeTime rawValue
  | otherwise = CalendarFloating <$> parseTimeM True defaultTimeLocale "%Y%m%dT%H%M%S" (Text.unpack rawValue)

parseRangeTime :: Text -> Maybe UTCTime
parseRangeTime rawValue =
  let value = Text.strip rawValue
   in firstJust
        [ parseUtc "%Y%m%dT%H%M%SZ" value
        , parseUtc "%Y-%m-%dT%H:%M:%SZ" value
        , parseUtc "%Y-%m-%d %H:%M:%SZ" value
        , parseFloating "%Y%m%dT%H%M%S" value
        , parseFloating "%Y-%m-%dT%H:%M:%S" value
        , parseFloating "%Y-%m-%d %H:%M:%S" value
        , (`UTCTime` 0) <$> parseDayText value
        ]

parseUtc :: String -> Text -> Maybe UTCTime
parseUtc format value =
  parseTimeM True defaultTimeLocale format (Text.unpack value)

parseFloating :: String -> Text -> Maybe UTCTime
parseFloating format value =
  localTimeToUTC utc <$> (parseTimeM True defaultTimeLocale format (Text.unpack value) :: Maybe LocalTime)

parseDayText :: Text -> Maybe Day
parseDayText value =
  firstJust
    [ parseTimeM True defaultTimeLocale "%Y%m%d" (Text.unpack value)
    , parseTimeM True defaultTimeLocale "%Y-%m-%d" (Text.unpack value)
    ]

looksLikeDate :: Text -> Bool
looksLikeDate value =
  (Text.length stripped == 8 && Text.all isDigit stripped)
    || (Text.length stripped == 10 && Text.index stripped 4 == '-' && Text.index stripped 7 == '-')
  where
    stripped = Text.strip value

parseDurationSeconds :: Text -> Maybe Int
parseDurationSeconds rawValue =
  let stripped = Text.strip rawValue
      (sign, value) =
        case Text.uncons stripped of
          Just ('-', rest) -> (-1, rest)
          Just ('+', rest) -> (1, rest)
          _ -> (1, stripped)
   in case Text.uncons value of
        Just ('P', rest) -> fmap (sign *) (parseDurationBody False 0 "" (Text.unpack rest))
        Just ('p', rest) -> fmap (sign *) (parseDurationBody False 0 "" (Text.unpack rest))
        _ -> Nothing

parseDurationBody :: Bool -> Int -> String -> String -> Maybe Int
parseDurationBody _ total [] [] =
  Just total
parseDurationBody _ _ _ [] =
  Nothing
parseDurationBody inTime total digits (character : rest)
  | isDigit character =
      parseDurationBody inTime total (digits <> [character]) rest
  | character == 'T' || character == 't' =
      parseDurationBody True total "" rest
  | otherwise = do
      number <- readMaybe digits
      seconds <-
        case toUpper character of
          'W' -> Just (number * 604800)
          'D' -> Just (number * 86400)
          'H' | inTime -> Just (number * 3600)
          'M' | inTime -> Just (number * 60)
          'S' | inTime -> Just number
          _ -> Nothing
      parseDurationBody inTime (total + seconds) "" rest

parseCalendarComponents :: Text -> [ICalComponent]
parseCalendarComponents body = filter (\c -> componentKind c `elem` ["VEVENT","VTODO"]) (concatMap flatten (parseCalendarTree body))
  where flatten c = c : concatMap flatten (componentChildren c)

parseCalendarTree :: Text -> [ICalComponent]
parseCalendarTree body = roots input
  where
    input = map (Text.dropWhileEnd (== '\r') . Text.pack) (unfoldLines (lines (Text.unpack body)))
    roots [] = []
    roots (line:rest) = case beginKind line of
      Just kind -> let (node,remaining) = nodeFrom kind [] [] rest in node : roots remaining
      Nothing -> roots rest
    nodeFrom kind properties children [] = (ICalComponent kind (reverse properties) (reverse children), [])
    nodeFrom kind properties children (line:rest)
      | endKind line == Just kind = (ICalComponent kind (reverse properties) (reverse children), rest)
      | Just childKind <- beginKind line = let (child,remaining) = nodeFrom childKind [] [] rest in nodeFrom kind properties (child:children) remaining
      | otherwise = nodeFrom kind (maybeToList (parseICalProperty line) ++ properties) children rest

beginKind :: Text -> Maybe Text
beginKind line =
  case Text.stripPrefix "BEGIN:" (Text.toUpper (Text.strip line)) of
    Just value -> Just value
    Nothing -> Nothing

endKind :: Text -> Maybe Text
endKind line =
  case Text.stripPrefix "END:" (Text.toUpper (Text.strip line)) of
    Just value -> Just value
    Nothing -> Nothing

parseICalProperty :: Text -> Maybe ICalProperty
parseICalProperty line =
  case Text.breakOn ":" line of
    (left, valueWithColon)
      | Text.null valueWithColon -> Nothing
      | otherwise ->
          let parts = Text.pack <$> splitQuoted ';' (Text.unpack left)
           in case parts of
                [] -> Nothing
                namePart : parameterParts ->
                  let rawName = Text.toUpper (lastNamePart namePart)
                      parameters = concatMap parseParameter parameterParts
                   in Just
                        ICalProperty
                          { propertyName = rawName
                          , propertyParameters = parameters
                          , propertyValue = Text.drop 1 valueWithColon
                          }

lastNamePart :: Text -> Text
lastNamePart value =
  case Text.splitOn "." value of
    [] -> value
    parts -> last parts

parseParameter :: Text -> [(Text, Text)]
parseParameter parameter =
  case Text.breakOn "=" parameter of
    (key, valueWithEquals)
      | Text.null valueWithEquals ->
          [(Text.toUpper key, "")]
      | otherwise ->
          [ (Text.toUpper key, stripQuotes value)
          | value <- Text.splitOn "," (Text.drop 1 valueWithEquals)
          ]

stripQuotes :: Text -> Text
stripQuotes value =
  fromMaybe value (Text.stripPrefix "\"" =<< Text.stripSuffix "\"" value)

firstProperty :: Text -> [ICalProperty] -> Maybe ICalProperty
firstProperty name =
  firstJust . fmap Just . propertiesNamed name

firstPropertyValue :: Text -> [ICalProperty] -> Maybe Text
firstPropertyValue name =
  fmap propertyValue . firstProperty name

propertiesNamed :: Text -> [ICalProperty] -> [ICalProperty]
propertiesNamed name =
  filter ((== Text.toUpper name) . propertyName)

hasParameterValue :: Text -> Text -> [(Text, Text)] -> Bool
hasParameterValue key expected =
  any (\(parameterKey, parameterValue) -> parameterKey == Text.toUpper key && parameterValue == Text.toUpper expected)

calendarTimeValueText :: CalendarTimeValue -> Text
calendarTimeValueText value =
  case value of
    CalendarDate day -> formatDayText day
    CalendarDateTime time -> formatUtcText time
    CalendarZoned _ time -> formatUtcText time
    CalendarFloating time -> Text.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" time)

formatDayText :: Day -> Text
formatDayText =
  Text.pack . formatTime defaultTimeLocale "%Y-%m-%d"

formatUtcText :: UTCTime -> Text
formatUtcText =
  Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

timeValueInstant :: CalendarTimeValue -> UTCTime
timeValueInstant value =
  case value of
    CalendarDate day -> UTCTime day 0
    CalendarDateTime time -> time
    CalendarZoned _ time -> time
    CalendarFloating time -> localTimeToUTC utc time

isDateValue :: CalendarTimeValue -> Bool
isDateValue value =
  case value of
    CalendarDate _ -> True
    _ -> False

shiftTimeValue :: NominalDiffTime -> CalendarTimeValue -> CalendarTimeValue
shiftTimeValue delta value =
  case value of
    CalendarDate day -> CalendarDate (utctDay (addUTCTime delta (UTCTime day 0)))
    CalendarDateTime time -> CalendarDateTime (addUTCTime delta time)
    CalendarZoned zone time -> CalendarZoned zone (addUTCTime delta time)
    CalendarFloating time -> CalendarFloating (addLocalTime delta time)

shiftTimeValueInstant :: NominalDiffTime -> CalendarTimeValue -> UTCTime
shiftTimeValueInstant delta =
  addUTCTime delta . timeValueInstant

rangeStartLt :: TimeRange -> UTCTime -> Bool
rangeStartLt timeRange value =
  maybe True (< value) (rangeStart timeRange)

rangeStartLe :: TimeRange -> UTCTime -> Bool
rangeStartLe timeRange value =
  maybe True (<= value) (rangeStart timeRange)

rangeEndGt :: TimeRange -> UTCTime -> Bool
rangeEndGt timeRange value =
  maybe True (> value) (rangeEnd timeRange)

rangeEndGe :: TimeRange -> UTCTime -> Bool
rangeEndGe timeRange value =
  maybe True (>= value) (rangeEnd timeRange)

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (value : values) =
  case value of
    Just found -> Just found
    Nothing -> firstJust values

calendarNameFromHref :: Text -> Text
calendarNameFromHref href =
  case Text.splitOn "/" href of
    _ : "dav" : "calendars" : calendarName : _ -> calendarName
    "dav" : "calendars" : calendarName : _ -> calendarName
    _ -> "calendar"

calendarIdFromHref :: Text -> Text
calendarIdFromHref href =
  case reverse (Text.splitOn "/" href) of
    fileName : _ ->
      fromMaybe fileName (Text.stripSuffix ".ics" fileName)
    [] ->
      href

