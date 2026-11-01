{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Vesbot.Currency
  ( getCurrency
  , updateCurrency
  , updateEmojis
  , getRichestUsers
  , currencyGuard
  , CurrencyUpdate(..)
  , EmojiUpdate(..)
  , CurrencyQuery(..)
  , EmojiQuery(..)
  , showC
  ) where

import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Config (CurrencyConfig(..))
import Vesbot.Utils (showT, liftIO)
import Vesbot.Database.Types (DBConnection)
import Vesbot.Currency.Database (getCurrency, updateCurrency, updateEmojis, getRichestUsers)
import Vesbot.Currency.Types (CurrencyUpdate(..), EmojiUpdate(..), CurrencyQuery(..), EmojiQuery(..))
import Vesbot.Currency.Commands (showC)

import DiscordWrapper.Discord.Internal.Types.Interactions.Interaction

import Discord
import Discord.Types
import Discord.Internal.Types.Interactions
import qualified Discord.Requests as R

import qualified Data.Text as T
import Vesbot.BotCommands.Types (SlashCommandInteraction (SlashCommandInteraction))


currencyGuard :: CurrencyConfig
              -> T.Text -- command name
              -> DBConnection
              -> UserId -- user to check the currency of
              -> Double -- cost to guard against
              -> SlashCommandInteraction -- interaction data so that we can send a response
              -> DiscordHandler () -- what to do if the guard is passed
              -> DiscordHandler ()
currencyGuard ccfg cmdName dbconn uid cost (SlashCommandInteraction common _ _) continue = do
  amt <- fmap currencyQueryAmount (liftIO $ getCurrency dbconn uid)
  if amt < cost
    then do
      Logging.log Info $ "User " <> showT uid <> " was too poor to use the `" <> cmdName <> "` command."
      let response = "You don't have enough money! This command costs " <> showC ccfg cost <> " and you only have " <> showC ccfg amt <> "."
      res <- restCall $ R.CreateInteractionResponse
        common.interactionId
        common.interactionToken
        (interactionResponseBasic response)
      case res of
        Left err ->
          Logging.log Error $
            "The REST call to send a currency guard response failed. Discord returned the error:\n" <> showT err
        Right _ -> return ()
    else do
      let cu = CurrencyUpdate uid (-1 * cost)
      liftIO $ updateCurrency dbconn cu 
      continue
