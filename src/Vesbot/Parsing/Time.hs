{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Vesbot.Parsing.Time
  ( parseDateTime
  , parseTimeOffset
  , dateTimeFormat
  , offsetFormat
  ) where

import qualified Data.Text as T
import Data.Time
import Control.Monad (when)
import Data.Either.Extra (mapLeft)
import Data.Tuple.Extra (first)
import qualified Data.Void (Void)
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.Char (isSpace)


import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P (decimal)

type Parser s = P.Parsec Data.Void.Void s 

displayParseError ::
  ( P.VisualStream s
  , P.TraversableStream s
  , P.ShowErrorComponent e
  ) => P.ParseErrorBundle s e -> T.Text
displayParseError e = T.pack $ "```\n" <> P.errorBundlePretty e <> "\n```"


dateTimeFormat :: String
dateTimeFormat = "MM-DD-YYYY hh:mm:ss <timezone>"
offsetFormat :: String
offsetFormat = "n[mo|w|d|h|min]"

-- | This function runs the parser pDateOrTime defined below and converts its result to a UTCTime. It requires the
-- current time to be specified, in case pDateOrTime returns just a (TimeOfDay, TimeZone) pair.
parseDateTime :: UTCTime -> String -> T.Text -> Either T.Text UTCTime
parseDateTime now name inpt = do
  utcOrLocal <- mapLeft displayParseError $
    P.parse (P.space *> pDateOrTime <* P.space <* P.eof) name inpt
  return $
    case utcOrLocal of
      Left (time, tz) ->
        let today = localDay $ utcToLocalTime tz now
        in  localTimeToUTC tz $ LocalTime today time
      Right utcTime -> utcTime

-- | This function runs the pOffsets parser defined below and extracts the Months, Weeks, Days, Hours, Minutes
-- that the user entered. It then adds this offset to the current time.
parseTimeOffset :: UTCTime -> String -> T.Text -> Either T.Text UTCTime
parseTimeOffset (UTCTime nowDay nowSeconds) name inpt = do
  offsets <-
    mapLeft displayParseError $
      P.parse (P.space *> pOffsets <* P.space <* P.eof) name inpt
  let (Offsets mo w d h mins) = foldr mergeOffsets (Offsets 0 0 0 0 0) offsets
      seconds = 60 * (60 * h + mins)
      newDay = addGregorianDurationRollOver (CalendarDiffDays mo (w * 7 + d)) nowDay
      newTime = addUTCTime (fromInteger seconds) (UTCTime newDay nowSeconds)
  return newTime

-- | Type to hold all of the offsets the user can enter.
-- Order: Months, Weeks, Days, Hours, Seconds
data Offsets = Offsets Integer Integer Integer Integer Integer

mergeOffsets :: Offsets -> Offsets -> Offsets
mergeOffsets (Offsets mo w d h m) (Offsets mo' w' d' h' m')
  = Offsets (mo + mo') (w + w') (d + d') (h + h') (m + m')

checkRange :: (Ord a, Show a, P.Stream s) => a -> a -> String -> Parser s a -> Parser s a
checkRange lo hi name p = do
  x <- p
  when (x < lo || x > hi) (fail $ unwords [name, " must be between ", show lo, " and ", show hi])
  return x

pOffsets :: Parser T.Text [Offsets]
pOffsets = P.sepBy1 pOffset P.space

pOffset :: Parser T.Text Offsets
pOffset = do
  n <- P.decimal
  unit <- pUnit
  return $ unit n
  where
    pUnit :: Parser T.Text (Integer -> Offsets)
    pUnit = (P.string "mo"  $> \mo -> Offsets mo 0 0 0 0) <|>
            (P.string "w"   $> \w  -> Offsets 0  w 0 0 0) <|>
            (P.string "d"   $> \d  -> Offsets 0  0 d 0 0) <|>
            (P.string "h"   $> \h  -> Offsets 0  0 0 h 0) <|>
            (P.string "min" $> \m  -> Offsets 0  0 0 0 m)

-- | This parser parses strings of the form "MM-DD-YYYY HH:MM <TZ>" where <TZ> is a timezone, and where some of the
-- fields are optional. The allowed formats are:
--   1. MM-DD-YYYY HH:MM <TZ>
--   2. MM-DD-YYYY HH:MM
--   3. MM-DD-YYYY
--   4. HH:MM <TZ>
--   5. HH:MM
-- In cases 2 and 5, the timezone defaults to UTC
-- In case 3, the time defaults to midnight UTC
--
-- If a date was specified, we compute the UTCTime (possibly with the defaults as above) and return it.
-- If a date was not specified, we simply return (TimeOfDay, TimeZone) (possibly with the defaults as above).
pDateOrTime :: Parser T.Text (Either (TimeOfDay, TimeZone) UTCTime)
pDateOrTime = do
  (date, time) <-
    P.try     (first Just                       <$> pDateAndTime)
    <|> P.try (first Just . (, TimeOfDay 0 0 0) <$> pDate       )
    <|> P.try (             (Nothing,)          <$> pTime       )
  P.space
  tz <- pTimeZone
  return $
    case date of
      Nothing  -> Left (time, tz)
      Just day ->
        let utcTime = localTimeToUTC tz $ LocalTime day time
        in  Right utcTime
  where
    pDate :: Parser T.Text Day
    pDate = P.label "date" $ do
      inpt <- T.unpack <$> P.takeWhile1P (Just "date") (not . isSpace)
      case (parseTimeM False defaultTimeLocale "%m-%d-%Y" inpt :: Maybe Day) of
        Just day -> return day
        Nothing -> fail $ "Invalid date (" <> inpt <> ")"

    pTime :: Parser T.Text TimeOfDay
    pTime =
      let pHour = P.label "hour" $ checkRange 0 23 "Hours" P.decimal
          pMinute = P.label "minute" $ checkRange 0 59 "Minutes" P.decimal
          pSecond = P.label "second" $ checkRange 0 59 "Seconds" P.decimal
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

    pDateAndTime :: Parser T.Text (Day, TimeOfDay)
    pDateAndTime = do
      date <- pDate
      P.space
      time <- pTime
      return (date, time)
