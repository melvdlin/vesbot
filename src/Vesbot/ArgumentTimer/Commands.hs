{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Vesbot.ArgumentTimer.Commands where

import Vesbot.Utils (showT, liftIO, void)
import Vesbot.Parsing (utcToInt)
import Vesbot.Config (Config(..))
import Vesbot.Database.Types (DBConnection)
import Vesbot.ArgumentTimer.Database (getLastArgument, updateArgumentTime)
import Vesbot.BotCommands.Types

import DiscordWrapper.Discord.Internal.Types.Interactions.Interaction.Kind.Channel
import DiscordWrapper.Discord.Internal.Types.Interactions.Interaction


import Discord
import Discord.Interactions as I
import qualified Discord.Requests as R

import Data.Time (getCurrentTime)


viewAC :: DBConnection -> Config -> BotCommand
viewAC dbconn cfg = SlashCommand
  { commandName = "viewac"
  , commandRegistration = I.createChatInput "viewac" "View the last obnoxious argument"
  , slashCommandHandler = \(SlashCommandInteraction common _ _) _ -> do
      mt <- liftIO . getLastArgument $ dbconn

      let reply = case mt of
            Nothing -> "There have been no obnoxious arguments so far!"
            Just (t, cid, mid) ->
              let timestamp = "<t:" <> (showT . utcToInt $ t) <> ":R>"
                  link = "https://discord.com/channels/" <> (showT . cfgGuildId $ cfg) <> "/" <> cid <> "/" <> mid
              in "The last obnoxious argument occured " <> timestamp <> ". You can view it here:\n" <> link

      void . restCall $
        R.CreateInteractionResponse
          common.interactionId
          common.interactionToken
          (interactionResponseBasic reply)
  }


resetAC :: DBConnection -> BotCommand
resetAC dbconn = MessageCommand
  { commandName = "resetac"
  , commandRegistration = reg
  , messageCommandHandler = \(MessageCommandInteraction common channelData cmdData) -> do
      now <- liftIO getCurrentTime
      let d = cmdData
          mcid = channelData.interactionChannelId
          mid = d.applicationCommandDataTargetMessageId
      reply <-
        case mcid of
          Nothing -> return "Error getting channel ID for the command."
          Just cid -> do
            liftIO $ updateArgumentTime dbconn now cid mid
            return "Time since the last obnoxious argument: 0 days..."
      void . restCall $
        R.CreateInteractionResponse
          common.interactionId
          common.interactionToken
          (interactionResponseBasic reply)
  }
  where
    reg = Just $ CreateApplicationCommandMessage "resetac" Nothing Nothing (Just False)
