{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Utils
  ( liftIO
  , forkIO
  , threadDelay
  , void
  , forM
  , forM_
  , fromBot
  , showT
  , find
  , traverse_
  , ephemeralResponseBasic
  , maybeToEither
  , left
  , right
  , findJust
  , announce
  , sendMessage
  , reactToMessage
  , ackButtonEditAndRemove
  ) where

import Vesbot.Logging as Logging (log, LogType(..))

import Discord
import Discord.Types
import Discord.Interactions
import qualified Discord.Requests as R
import qualified Data.Text as T

import Control.Monad.IO.Class (liftIO)
import UnliftIO.Concurrent (forkIO, threadDelay)
import Control.Monad (void, forM, forM_, join)
import Data.List (find)
import Data.Foldable (traverse_)
import Data.Maybe (isJust)


showT :: Show a => a -> T.Text
showT = T.show


fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

left :: Either a b -> Maybe a
left = either Just (const Nothing)

right :: Either a b -> Maybe b
right = either (const Nothing) Just

findJust :: Foldable t => t (Maybe a) -> Maybe a
findJust = join . find isJust

-- | make a message ephemeral
makeEphemeral :: InteractionResponseMessage -> InteractionResponse
makeEphemeral mess =
  InteractionResponseChannelMessage
    mess
      { interactionResponseMessageFlags =
          Just (InteractionResponseMessageFlags [InteractionResponseMessageFlagEphermeral])
      }

ephemeralResponseBasic :: T.Text -> InteractionResponse
ephemeralResponseBasic = makeEphemeral . interactionResponseMessageBasic


-- | Basic response type functions.

announce :: ChannelId -> T.Text -> DiscordHandler ()
announce acid content = do
    r <- restCall $ R.CreateMessage acid content
    case r of
      Left err -> Logging.log Error $ T.intercalate "\n"
        [ "Failed to send an announcement."
        , logInfo
        , "Discord returned the error:\n", showT err
        ]
      _ -> Logging.log Info $ T.intercalate "\n"
        [ "Successfully sent an announcement."
        , logInfo
        ]
  where
    logInfo = T.intercalate "\n"
      [ "CHANNEL: " <> showT acid
      , "CONTENT: " <> content
      ]

reactToMessage :: Message -> T.Text -> DiscordHandler ()
reactToMessage msg eid = void . restCall $ R.CreateReaction (messageChannelId msg, messageId msg) eid

sendMessage :: ChannelId -> Maybe MessageId -> T.Text -> DiscordHandler ()
sendMessage cid replyTo content =
  let ref = case replyTo of
        Nothing  -> Nothing
        Just mid -> Just (MessageReference (Just mid) (Just cid) Nothing False)
      opts = R.MessageDetailedOpts content False Nothing Nothing [] Nothing ref Nothing Nothing
  in do
    r <- restCall $ R.CreateMessageDetailed cid opts
    case r of
      Left err -> Logging.log Error $ T.intercalate "\n"
        [ "Error sending a message."
        , logInfo
        , "Discord returned the error:\n" <> showT err
        ]
      _ -> Logging.log Info $ T.intercalate "\n"
        [ "Successfully sent a message."
        , logInfo
        ]
  where
    logInfo = T.intercalate "\n"
      [ "CHANNEL: " <> showT cid
      , "ID OF MESSAGE REPLYING TO: " <> showT replyTo
      , "CONTENT: " <> content
      ]

ackButtonEditAndRemove :: InteractionId -> InteractionToken -> Message -> T.Text -> DiscordHandler ()
ackButtonEditAndRemove iid itok omsg addition =
  let ocontent = messageContent omsg
      reply = ocontent <> addition
      response = InteractionResponseMessage (Just False) (Just reply) Nothing Nothing Nothing (Just []) Nothing []
  in do
    result <- restCall $ R.CreateInteractionResponse iid itok (InteractionResponseUpdateMessage response)
    case result of
      Left err -> Logging.log Error $ T.intercalate "\n"
        [ "Error editing the message a button is attached to."
        , logInfo
        , "Discord Returned the error:\n" <> showT err
        ]
      _        -> Logging.log Info $ T.intercalate "\n"
        [ "Edited a message a button was attached to."
        , logInfo
        ]
  where
    logInfo = "MESSAGE ID: " <> (showT . messageId $ omsg)
