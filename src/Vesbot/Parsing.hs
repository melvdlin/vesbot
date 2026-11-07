{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Parsing where

import Vesbot.Logging as Logging (log, LogType(..))

import Discord.Types
import Discord.Internal.Types.Interactions

import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Int (Int64)
import Data.Scientific (toRealFloat)
import Vesbot.Utils (maybeToEither)


intToUTC :: Int64 -> UTCTime
intToUTC = posixSecondsToUTCTime . realToFrac

utcToInt :: UTCTime -> Int64
utcToInt = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds


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
