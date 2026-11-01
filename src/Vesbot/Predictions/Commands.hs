{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vesbot.Predictions.Commands
  ( makePrediction
  ) where

import Vesbot.Utils (showT, maybeToEither, ephemeralResponseBasic, void, liftIO)
import Vesbot.Config (Env(..))
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Parsing (extractNumberOption, extractStringOption, parseDateInput, extractField, utcToInt, getUserData, extractOptionsData)
import Vesbot.Actions (Action(..))
import Vesbot.Predictions.Types
import Vesbot.Predictions.Database
import Vesbot.BotCommands.Types (BotCommand(..), SlashCommandInteraction (SlashCommandInteraction))

import DiscordWrapper.Discord.Internal.Types.Interactions.Interaction.Kind.Channel
import DiscordWrapper.Discord.Internal.Types.Interactions.Interaction

import JSONScheduler.Scheduler (Schedule(..), scheduleJob)

import Discord
import Discord.Types
import Discord.Internal.Types.Interactions (OptionsData(..), InteractionResponse(..), interactionResponseBasic, ApplicationCommandData(..))
import Discord.Internal.Types.ApplicationCommands
import qualified Discord.Requests as R
import Data.Time
import qualified Data.Text as T


parsePredictionCommand :: UTCTime -> SlashCommandInteraction -> Maybe OptionsData -> Either T.Text (PredictionData, ChannelId)
parsePredictionCommand
  currTime
  (SlashCommandInteraction _ channelData cmdData)
  opts = do
    let cmdName = cmdData.applicationCommandDataName

    optValues <- extractOptionsData cmdName opts
    let confidence = extractNumberOption "confidence" optValues -- it's ok if it fails since it's optional
    predText <- extractField extractStringOption "prediction" cmdName optValues
    dueDateText <- extractField extractStringOption "date" cmdName optValues
    _ <- maybeToEither
          "Error accessing Guild ID for the prediction command. Note that this command can only be used in a server."
          channelData.interactionGuildId
    cid <-
      maybeToEither
        "Error accessing channel ID for the prediction command. Note that this command can only be used in a server."
        channelData.interactionChannelId
    uid <-
      maybeToEither
        "Could not get user data for the prediction command. Note that this command can only be used in a server."
        (getUserData channelData.interactionUser)
    dueDateUTC <-
      maybeToEither
        (T.intercalate "\n"
          [ "Error parsing prediction date. Here are the formatting options."
          , "**Relative time:** \"in #w\" or \"in #d\" or \"in #h\" or \"in #m\", where # is a positive integer.\
          \ \"w\" stands for weeks, \"d\" stands for days, \"h\" stands for hours, and \"m\" stands for minutes."
          , "**Absolute time:** \"on month-day-year HH:MM <timezone>\", where the timezone is optional.\
          \If no timezone is entered, the bot will default to UTC. Timezone examples are CST, CDT, EST, etc."
          ]
        )
        (parseDateInput currTime . T.unpack $ dueDateText)
    return
      ( PredictionData
          { predictionUserId = uid
          , predictionContent = predText
          , predictionConfidence = confidence
          , predictionMadeDate = currTime
          , predictionDueDate = dueDateUTC
          }
      , cid
      )

-- | Generate the bot reply to the command. parsePredictionCommand returns either an error message or a record of the
-- relevant prediction command data. We must either respond with an (ephemeral) error, or else form the bot reply
-- in the case that the user actually predicted a future event.
createInitialPredictionResponse :: UTCTime -> Either T.Text (PredictionData, ChannelId) -> InteractionResponse
createInitialPredictionResponse currTime mpcd =
  case mpcd of
    Right (pd,_) ->
      if currTime < predictionDueDate pd then
        let
          userPing = "<@" <> showT (predictionUserId pd) <> ">"
          dueDateUTC = showT . utcToInt $ predictionDueDate pd
          dueDateTimeStampRel = "<t:" <> dueDateUTC <> ":R>"
          dueDateTimeStampAbs = "<t:" <> dueDateUTC <> ":f>"
          responseStart = case predictionConfidence pd of
            Nothing -> "They claim"
            Just c  -> "They are " <> showT c <> "%" <> " confident"
          response = T.intercalate "\n"
            [ userPing <> " has made a prediction!"
            , responseStart <> " that on or before " <> dueDateTimeStampAbs <> ", which is " <> dueDateTimeStampRel <>
            ", the following will occur:"
            , "*" <> predictionContent pd <> "*."
            ]
        in
          interactionResponseBasic response
      else
        ephemeralResponseBasic "You can only predict events in the future!"
    Left err -> ephemeralResponseBasic err


makePrediction :: Env -> BotCommand
makePrediction env = SlashCommand
  { commandName = "predict"
  , commandRegistration = Just reg
  , slashCommandHandler = \intr@(SlashCommandInteraction common _ _) opts -> do
      currTime <- liftIO getCurrentTime
      let iTok = common.interactionToken -- save the interaction token to get the message ID of the bot's reply
      let mpd = parsePredictionCommand currTime intr opts :: Either T.Text (PredictionData, ChannelId)
      let botReply = createInitialPredictionResponse currTime mpd

      -- Respond with the error or the correct reply
      void . restCall $
        R.CreateInteractionResponse
          common.interactionId
          common.interactionToken
          botReply

      -- | If the predictionCommandData was parsed correctly, we want to save the prediction to the database
      -- We can't do this before because we also want to save the ID of the message the bot responds to the
      -- slash command with, and this message is sent **after** the restCall immediately above.
      case mpd of
        Right (pd, cid) | currTime < predictionDueDate pd -> do
          maybeResult <- restCall $
            R.GetOriginalInteractionResponse
              common.interactionApplicationId
              iTok
          case maybeResult of
            Right msg -> do
              void . liftIO $ do
                pid <- savePrediction (envDBConnection env) pd
                let a = AnnounceDuePrediction pid cid (messageId msg)
                r <- scheduleJob (envScheduler env) (Just $ "prediction:" <> showT pid) (Once . predictionDueDate $ pd) a
                case r of
                  Left err -> Logging.log Error $ "Error scheduling a prediction announcement. \
                    \The job scheduler returned the following error:\n" <> err
                  _ -> return ()
            Left err -> Logging.log Error $ "Error acquiring the bot's prediction reply message.\
              \Discord sent the following error message:\n" <> showT err
        _ -> return ()
  }
    where
      predictionField = OptionValueString "prediction" Nothing "What you are predicting will happen" Nothing True (Left False) (Just 1) Nothing
      dateField = OptionValueString "date" Nothing "\"in #[w|d|h|m]\" or \"on MM-DD-YYYY HH:MM <timezone>\"" Nothing True (Left False) (Just 1) Nothing
      confidenceField =  OptionValueNumber "confidence" Nothing "(Optional) Prediction confidence (ex: 20.4 means 20.4%)" Nothing False (Left False) (Just 0) (Just 100)
      reg = -- Command registration
        CreateApplicationCommandChatInput
          "predict"
          Nothing
          "Predict the future!"
          Nothing
          (Just $ OptionsValues [predictionField, dateField, confidenceField ])
          Nothing
          (Just False)
