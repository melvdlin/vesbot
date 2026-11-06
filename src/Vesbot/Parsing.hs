{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Vesbot.Parsing where

import Vesbot.Logging as Logging (log, LogType(..))

import Discord.Types
import Discord.Internal.Types.Interactions

import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import qualified Data.Text as T

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P (decimal)

import qualified Data.Void (Void)

import Data.Functor (($>))
import Control.Applicative ((<|>))

import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Int (Int64)
import Data.Char (isDigit)
import Data.Scientific (toRealFloat)
import Vesbot.Utils (maybeToEither)
import Control.Monad (when)
import Data.Either.Extra (mapLeft, eitherToMaybe)
import Data.Tuple.Extra (first)
import Data.Maybe (fromMaybe, isNothing)

type Parser s = P.Parsec Data.Void.Void s 

intToUTC :: Int64 -> UTCTime
intToUTC = posixSecondsToUTCTime . realToFrac

utcToInt :: UTCTime -> Int64
utcToInt = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

defHead :: a -> [a] -> a
defHead d [] = d
defHead _ (x:_) = x


parseJSON :: AE.FromJSON a => FilePath -> IO a
parseJSON path = do
  jsonData <- BS.readFile path
  let mdecoded = AE.eitherDecodeStrict jsonData
  case mdecoded of
    Left err -> do
      Logging.log Error $ "Error parsing the JSON data in " <> T.pack path <> ". Error was:\n" <> T.pack err
      error "fatal error"
    Right d  -> do
      Logging.log Info $ "Successfully parsed the JSON data in " <> T.pack path <> "."
      return d


-- | Kind of a silly function, date input parsing for the predictions is fairly restrictive right now.
parseDateInput :: UTCTime -> String -> Maybe UTCTime
parseDateInput currTime inpt = case words inpt of
  ("on":absInpt:timeInpt:timeZoneOrEmpty) ->
    let
      timeZone = parseTimeM False defaultTimeLocale "%Z" (defHead "UTC" timeZoneOrEmpty)
    in do
      date <- parseTimeM False defaultTimeLocale "%m-%d-%Y" absInpt :: Maybe Day
      time <- parseTimeM False defaultTimeLocale "%R" timeInpt :: Maybe TimeOfDay
      tz <- timeZone
      return (localTimeToUTC tz (LocalTime date time))

  ("in":relInpt:_) -> do
    let letter = dropWhile isDigit relInpt
    timeIncrement <- parseTimeM False defaultTimeLocale (concat ["%", letter, letter]) relInpt :: Maybe NominalDiffTime
    return (addUTCTime timeIncrement currTime)

  _          -> Nothing

{-# ANN parseDueDate ( "HLint: ignore Redundant return" :: String ) #-}
parseDueDate :: UTCTime -> T.Text -> OptionsData -> Either T.Text UTCTime
parseDueDate now cmdName opts = do
    let rawDue = eitherToMaybe $ extractField extractStringOption "on" cmdName opts
    let rawOffset = eitherToMaybe $ extractField extractStringOption "in" cmdName opts
    if isNothing rawDue && isNothing rawOffset
      then Left "at least one of `on` or `in` must be specified"
      else do
        due  <- maybe (Right now) (parseDateTime now "on") rawDue
        due' <- maybe (Right due) (parseOffset now "in") rawOffset
        return due'

dateTimeFormat :: String
dateTimeFormat = "MM-DD-YYYY hh:mm:ss <timezone>"
offsetFormat :: String
offsetFormat = "n[m|w|d|h|min]"

parseDateTime :: UTCTime -> String -> T.Text -> Either T.Text UTCTime
parseDateTime now name s = do
  (day, time, tz) <- mapLeft displayParseError $ P.parse
    (P.space *> pDateTime <* P.space <* P.eof)
    name s
  let day' = fromMaybe (localDay $ utcToLocalTime tz now) day
  return $ localTimeToUTC tz $ LocalTime day' time

displayParseError ::
  ( P.VisualStream s
  , P.TraversableStream s
  , P.ShowErrorComponent e
  ) => P.ParseErrorBundle s e -> T.Text
displayParseError e = T.pack $ "```\n" <> P.errorBundlePretty e <> "\n```"

parseOffset :: UTCTime -> String -> T.Text -> Either T.Text UTCTime
parseOffset now name s = do
  offsets <- mapLeft displayParseError $ P.parse
    (P.space *> pOffsets <* P.space <* P.eof)
    name s
  let tz = utc
  let (LocalTime day time) = utcToLocalTime tz now
  let (Offsets m w d h mins) = foldr mergeOffsets (Offsets 0 0 0 0 0) offsets
  let seconds = 60 * (60 * h + mins)
  let day' = addGregorianDurationRollOver (CalendarDiffDays m (w * 7 + d)) day
  let date' = localTimeToUTC tz $ LocalTime day' time
  let date'' = addUTCTime (fromInteger seconds) date'

  return date''

data Offsets = Offsets
  -- Months
  Integer
  -- Weeks
  Integer
  -- Days
  Integer
  -- Hours
  Integer
  -- Minutes
  Integer
data Offset = MonthOffset Integer | WeekOffset Integer | DayOffset Integer | HourOffset Integer | MinuteOffset Integer

mergeOffsets :: Offset -> Offsets -> Offsets
mergeOffsets (MonthOffset  m'   ) (Offsets m w d h min') = Offsets (m + m') w d h min'
mergeOffsets (WeekOffset   w'   ) (Offsets m w d h min') = Offsets m (w + w') d h min'
mergeOffsets (DayOffset    d'   ) (Offsets m w d h min') = Offsets m w (d + d') h min'
mergeOffsets (HourOffset   h'   ) (Offsets m w d h min') = Offsets m w d (h + h') min'
mergeOffsets (MinuteOffset min'') (Offsets m w d h min') = Offsets m w d h (min' + min'')

checkRange :: (Ord a, Show a, P.Stream s) => a -> a -> String -> Parser s a -> Parser s a
checkRange lo hi name p = do
  x <- p
  when (x < lo || x > hi) (fail $ unwords [name, "must be between", show lo, "and", show hi])
  return x

pOffsets :: Parser T.Text [Offset]
pOffsets = P.sepBy1 pOffset P.space

pOffset :: Parser T.Text Offset
pOffset = let
  pUnit :: Parser T.Text (Integer -> Offset)
  pUnit = (P.string "m"   $> MonthOffset ) <|>
          (P.string "w"   $> WeekOffset  ) <|>
          (P.string "d"   $> DayOffset   ) <|>
          (P.string "h"   $> HourOffset  ) <|>
          (P.string "min" $> MinuteOffset) 
  in do
    n <- P.decimal
    unit <- pUnit
    return $ unit n

pDateTime :: Parser T.Text (Maybe Day, TimeOfDay, TimeZone)
pDateTime = let
  pDate :: Parser T.Text Day
  pDate = let
    pMonth = P.label "month" $ checkRange 1 12 "month" P.decimal 
    pDay = P.label "day" P.decimal 
    pYear = P.label "year" P.decimal
   in P.label "date" $ do
    month <- pMonth
    _ <- P.string "-"
    day <- pDay
    _ <- P.string "-"
    year <- pYear
    case fromGregorianValid year month day of
      Just day' -> return day'
      Nothing   -> fail $ "invalid day (" <> show day <> ")"
  pTime :: Parser T.Text TimeOfDay
  pTime = let
    pHour = P.label "hour" $ checkRange 0 23 "hour" P.decimal 
    pMinute = P.label "minute" $ checkRange 0 59 "minute" P.decimal
    pSecond = P.label "second" $ checkRange 0 59 "second" P.decimal
   in P.label "time of day" $ do
    hour <- pHour
    _ <- P.string ":"
    minute <- pMinute
    second <- P.string ":" *> pSecond <|> return 0
    return $ TimeOfDay hour minute second
  pTimeZone :: Parser T.Text TimeZone
  pTimeZone = P.label "time zone" $ do
    s <- P.takeRest
    if s == ""
      then return utc
      else parseTimeM False defaultTimeLocale "%Z" $ T.unpack s 

  pDateAndTime = do
    date <- pDate
    P.space
    time <- pTime
    return (date, time)
  in do
    (date, time) <-  P.try (first Just                       <$> pDateAndTime)
                 <|> P.try (first Just . (, TimeOfDay 0 0 0) <$> pDate       )
                 <|> P.try (             (Nothing,)          <$> pTime       )
    P.space
    tz <- pTimeZone
    return (date, time, tz)

-- | Extracting otions from command fields
extractStringOption :: T.Text -> OptionsData -> Maybe T.Text
extractStringOption _ (OptionsDataSubcommands _) = Nothing
extractStringOption optionName (OptionsDataValues vs) =
  case [v | OptionDataValueString n (Right v) <- vs, n == optionName] of
    (v:_) -> Just v
    _     -> Nothing

extractIntegerOption :: T.Text -> OptionsData -> Maybe Integer
extractIntegerOption _ (OptionsDataSubcommands _) = Nothing
extractIntegerOption optionName (OptionsDataValues vs) =
  case [v | OptionDataValueInteger n (Right v) <- vs, n == optionName] of
    (v:_) -> Just v
    _     -> Nothing

extractNumberOption :: T.Text -> OptionsData -> Maybe Double
extractNumberOption _ (OptionsDataSubcommands _) = Nothing
extractNumberOption optionName (OptionsDataValues vs) =
  case [v | OptionDataValueNumber n (Right v) <- vs, n == optionName] of
    (v:_) -> Just (toRealFloat v)
    _     -> Nothing

extractUserOption :: T.Text -> OptionsData -> Maybe UserId
extractUserOption _ (OptionsDataSubcommands _) = Nothing
extractUserOption optionName (OptionsDataValues vs) =
  case [uid | OptionDataValueUser n uid <- vs, n == optionName] of
    (uid:_) -> Just uid
    _     -> Nothing

extractField :: (T.Text -> OptionsData -> Maybe a) -- function to extract the field with
             -> T.Text -- field to extract
             -> T.Text -- command name
             -> OptionsData
             -> Either T.Text a -- either error message or the thing to be extracted
extractField f field cmd = maybeToEither err . f field
  where
    err = "Could not access `" <> field <> "` field in the `" <> cmd <> "` command"

extractOptionsData :: T.Text -> Maybe OptionsData -> Either T.Text OptionsData
extractOptionsData cmd = maybeToEither ("Could not access option data for `" <> cmd <> "` command")

-- Helper function that extracts the UserId from a MemberOrUser type
getUserData :: MemberOrUser -> Maybe UserId
getUserData (
  MemberOrUser (
    Left (
      GuildMember
        { memberUser = Just (
            User { userId = uid }
          )
        }
      )
    )
  ) = Just uid

getUserData _ = Nothing


-- turn an emoji into its text based name
emojiToText :: Emoji -> T.Text
emojiToText (Emoji meid name _ _ _ _) =
  case meid of
    Nothing -> name
    Just eid -> "<:" <> name <> ":" <> T.show eid <> ">"
