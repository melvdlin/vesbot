{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Vesbot.Handler.Interactions
  ( interactionHandler
  ) where

import Vesbot.Config (Config(..), Env(..))
import Vesbot.Utils (find, showT)
import Vesbot.Parsing (getUserData)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.BotCommands as BC
import Vesbot.Predictions (predictionButtonResponse)
import Vesbot.Predictions.Types (PredictionId)
import Discord
import Discord.Types
import Discord.Interactions

import qualified Data.Text as T
import Vesbot.BotCommands.Types (SlashCommandInteraction (SlashCommandInteraction))

-- | Only supports application commands currently. When someone uses an application command, the
-- function tries to look it up in the list of the registered commands.
interactionHandler :: Config -> Env -> SlashCommandInteraction -> DiscordHandler ()
interactionHandler cfg env =
  \case
    cmd@(SlashCommandInteraction common channelData cmdData) -> case cmdData.applicationCommandData of
      slashInput@ApplicationCommandDataChatInput {} ->
        case find (\c -> applicationCommandDataName slashInput == commandName c) (BC.slashCommands env cfg) of
          Just found -> do
            Logging.log Info $ T.intercalate "\n"
              [ "Recieved a slash command from a user."
              , "COMMAND NAME: " <> applicationCommandDataName slashInput
              ]
            BC.slashCommandHandler found cmd (optionsData slashInput)
          Nothing ->
            Logging.log Warn $
              "Slash command " <> (showT . applicationCommandDataName $ slashInput) <> " is unknown (registrations out of date?)"
      msgInput@ApplicationCommandDataMessage {} ->
        case find (\c -> applicationCommandDataName msgInput == commandName c) (BC.messageCommands env) of
          Just found -> do
            Logging.log Info $ T.intercalate "\n"
              [ "Recieved a message command from a user."
              , "COMMAND NAME: " <> applicationCommandDataName msgInput
              ]
            BC.messageCommandHandler found cmd
          Nothing ->
            Logging.log Warn $
              "Message command " <> (showT . applicationCommandDataName $ msgInput) <> " is unknown (registrations out of date?)"
      _ -> return () -- unsupported ApplicationCommand type
    i@InteractionComponent { componentData = ButtonData {} } -> buttonInteractionHandler env i
    _ -> return () -- Unexpected/unsupported interaction type

--TODO: Really needs a more advanced parsing system for more button types.
buttonInteractionHandler :: Env -> SlashCommandInteraction -> DiscordHandler ()
buttonInteractionHandler env =
  \case
    InteractionComponent
      { componentData = bdata@ButtonData {}
      , interactionId = iid
      , interactionToken = itok
      , interactionMessage = msg
      , interactionUser = mou
      } ->
        case T.splitOn ":" . componentDataCustomId $ bdata of
          ["prediction", pid, torf] -> do
            let muid = getUserData mou
            case muid of
              Nothing ->
                Logging.log Warn $ T.intercalate "\n"
                  [ "Could not get the user ID of a user that clicked a prediction result button. \
                   \Note that this bot is desined to be used in a server."
                  , "MESSAGE ID: " <> (showT . messageId $ msg)
                  ]
              Just uid -> do
                predictionButtonResponse (envDBConnection env) iid itok (read (T.unpack pid) :: PredictionId) uid msg torf
          _  -> return()
    _ -> return ()
