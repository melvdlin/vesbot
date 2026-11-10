{-# LANGUAGE OverloadedStrings #-}
module Vesbot.BotCommands
  ( BotCommand(..)
  , slashCommands
  , messageCommands
  ) where


import Vesbot.Logging as Logging (LogType(..), log)
import Vesbot.Utils (showT, void, liftIO, ephemeralResponseBasic, forM_, threadDelay, announce)
import Vesbot.Parsing (extractIntegerOption, extractOptionsData, extractField, extractStringOption)
import Vesbot.Config (Env(..), Config(..))
import Vesbot.BotCommands.Types
import Vesbot.ArgumentTimer.Commands (viewAC, resetAC)
import Vesbot.Predictions.Commands (makePrediction)
import Vesbot.Currency (currencyGuard, showC)
import Vesbot.Currency.Types (EmojiUpdate(..))
import Vesbot.Currency.Database (updateEmojis)
import Vesbot.Currency.Commands (viewCurrencyCommand, payCommand, viewRichestUsersCommand, viewEmojisCommand)
import Vesbot.Reminders.Commands (setReminder)
import Vesbot.Responses (RareEmoji(..), selectNRandomRareEmojis )
import qualified Vesbot.BigRoll as Roll

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.Interactions

import qualified Data.Either.Extra as Either

import qualified Text.Megaparsec as MP
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import System.Random.Stateful (uniformRM, globalStdGen, getStdRandom)
import Control.Monad (when)
import qualified Vesbot.BigRoll as Expr


-- | Some are slash commands, some are message commands.
slashCommands :: Env -> Config -> [BotCommand]
slashCommands env cfg =
  [ ping
  , time
  , glorp
  , roll
  , gamble cfg env
  , viewAC (envDBConnection env) cfg
  , makePrediction env
  , setReminder env
  , viewCurrencyCommand (envDBConnection env) cfg
  , payCommand (envDBConnection env) cfg
  , viewRichestUsersCommand (envDBConnection env) cfg
  , viewEmojisCommand (envDBConnection env)
  ]

messageCommands :: Env -> [BotCommand]
messageCommands env =
  [ resetAC (envDBConnection env)
  ]


ping :: BotCommand
ping =
  basicSlashCommand
    "ping"
    "Responds 'pong' if the bot is alive"
    (return "pong!")


time :: BotCommand
time =
  basicSlashCommand
    "time"
    "Displays the Current time in UTC"
    (do x <- getCurrentTime; return $ "The current time is " <> showT x <> ".")


glorp :: BotCommand
glorp =
  basicSlashCommand
    "glorp"
    "Prints a giant glorp"
    (return $ ".\n" <> giantGlorp)
  where
    giantGlorp :: T.Text
    giantGlorp = "I⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⢠⣄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⠄⠘⢻⠄⠄⠄⠄⠄⠄⠄⢡⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⠄⠄⠘⡀⠄⠄⠄⠄⠄⠄⠘⡆⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⠄⠄⠄⣇⠄⠄⣀⣀⣀⣤⣄⣷⣤⡀⠄⠄⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⠄⣀⣸⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⣠⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⢀⣸⣿⣿⣿⣿⣿⢹⣿⣿⣿⣿⣿⣿⠹⣿⣿⣷⣤⡄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠰⣿⣍⡻⣿⠿⠿⠾⢾⣿⣿⣿⣿⣿⣿⠿⠛⠻⢿⡟⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⣿⣏⣰⣿⣦⣀⠄⠄⢹⣿⣿⣿⣿⣏⡀⠄⣀⣿⣿⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠿⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣟⣫⣭⡄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠸⠿⠶⠶⢿⣿⣿⣿⣿⣿⣅⣖⣼⣿⣿⣿⣯⣽⣭⠉⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⢸⣿⣿⣿⣿⣿⣿⣿⣿⣭⣤⣽⣿⣿⣿⣿⢿⣻⡇⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⣼⣿⣿⣿⣿⣿⣿⡿⢿⡿⣿⢿⣟⣛⣿⣷⣿⣿⣿⣂⠄⠄I\n"
              <> "I⠄⠄⠄⠄⣸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⡆⠄I\n"


roll :: BotCommand
roll = SlashCommand
  { commandName = cmdName
  , commandRegistration = Just reg
  , slashCommandHandler = \intr opts -> do
    let exprOrErr = parseCommand intr opts
    case exprOrErr of
      Left err -> do
        void . restCall $
          R.CreateInteractionResponse
            (interactionId intr)
            (interactionToken intr)
            (ephemeralResponseBasic err) 
      Right e -> do
        mUsr <- liftIO $ tryToGetUser (interactionUser intr) -- this command is only going to work for guild members
        case mUsr of
          Nothing -> return ()
          Just _ -> do
            sendRollReply e intr
  }
  where
    cmdName = "roll"
    opt = OptionValueString "roll" Nothing "The expression to roll" Nothing True (Left False) (Just 1) Nothing
    reg = -- Command registration
      CreateApplicationCommandChatInput
        cmdName
        Nothing
        "Roll some dice"
        Nothing 
        (Just $ OptionsValues [opt])
        Nothing
        (Just False)

    parseCommand :: Interaction -> Maybe OptionsData -> Either T.Text (Roll.Expr Integer)
    parseCommand InteractionApplicationCommand {} opts = do
      optValues <- extractOptionsData cmdName opts
      s <- TE.encodeUtf8 <$> extractField extractStringOption "roll" cmdName optValues
      Either.mapLeft
        (T.pack . ("```\n" <>) . ("\n```" <>) . MP.errorBundlePretty)
        (MP.parse Roll.parse "roll" s)
      
    parseCommand _ _ = Left "Tried to parse a non slash command on an interaction that is not an application command."

    tryToGetUser :: MemberOrUser -> IO (Maybe User)
    tryToGetUser mou = case mou of
      MemberOrUser (Right _) -> do
        liftIO $ Logging.log Warn $
          "User that rolled a die was not a member of a guild. Note that the bot is only\
          \ designed to be used in a server."
        return Nothing
      MemberOrUser (Left gm) ->
        case memberUser gm of
          Nothing -> do
            Logging.log Error $
              "Could not obtain the user data of a user that rolled a die."
            return Nothing
          Just u -> return (Just u)

    sendRollReply :: Roll.Expr Integer -> Interaction -> DiscordHandler ()
    sendRollReply e intr = do
      rolled <- liftIO $ getStdRandom $ Roll.roll e
      let preciseExpr = Roll.preciseExpr e
      let prettyExpr = Roll.prettyExpr e
      let pretty = Roll.pretty rolled
      let result = Roll.eval rolled
      let resultDisplay = case rolled of
            (Expr.RolledConstExpr _) -> show result
            (Expr.RolledDieExpr   _) -> show result
            _ -> unwords [pretty, "=", show result]
      let response = T.pack $ unwords [prettyExpr, "⟹", resultDisplay]
      Logging.log Info $ T.pack $ unwords ["User rolled", preciseExpr, "=", prettyExpr, "=>", pretty, "=", show result]

      res <- restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic response)
      case res of
        Left err ->
          Logging.log Error $
            "The REST call to send a roll response failed. Discord returned the error:\n" <> showT err
        Right _ -> return ()

gamble :: Config -> Env -> BotCommand
gamble cfg env = SlashCommand
  { commandName = cmdName
  , commandRegistration = Just reg
  , slashCommandHandler = \intr opts -> do
    let intOrErr = parseCommand intr opts
    case intOrErr of
      Left err -> do
        void . restCall $
          R.CreateInteractionResponse
            (interactionId intr)
            (interactionToken intr)
            (ephemeralResponseBasic err) 
      Right (n, mCall) -> do
        mUsr <- liftIO $ tryToGetUser (interactionUser intr) -- this command is only going to work for guild members
        case mUsr of
          Nothing -> return ()
          Just usr -> do
            let uid = userId usr
            currencyGuard (cfgCurrencyInfo cfg) cmdName (envDBConnection env) uid cmdCost intr $ do
            rollResult <- rollResponse n mCall intr

            -- get a prize if you meet the win condition defined below
            when (winCondition n mCall rollResult) $
              onWin intr uid n mCall
  }
  where
    cmdName = "gamble"
    cmdCost = 0.02
    minRoll = 50
    opt1 = OptionValueInteger "n" Nothing "The number of sides the die has (must be >= 50)" Nothing True (Left False) (Just minRoll) Nothing
    opt2 = OptionValueInteger "call" Nothing "(Optional) What you think you'll roll" Nothing False (Left False) (Just 0) Nothing
    reg = -- Command registration
      CreateApplicationCommandChatInput
        cmdName
        Nothing
        ("Gamble by rolling an n-sided die. Cost: " <> showC (cfgCurrencyInfo cfg) cmdCost)
        Nothing 
        (Just $ OptionsValues [opt1, opt2])
        Nothing
        (Just False)

    parseCommand :: Interaction -> Maybe OptionsData -> Either T.Text (Integer, Maybe Integer)
    parseCommand InteractionApplicationCommand {} opts = do
      optValues <- extractOptionsData cmdName opts
      n <- extractField extractIntegerOption "n" cmdName optValues
      let call = extractIntegerOption "call" optValues
      return (n, call)
    parseCommand _ _ = Left "Tried to parse a non slash command on an interaction that is not an application command."

    tryToGetUser :: MemberOrUser -> IO (Maybe User)
    tryToGetUser mou = case mou of
      MemberOrUser (Right _) -> do
        liftIO $ Logging.log Warn $
          "User that rolled a die was not a member of a guild. Note that the bot is only\
          \ designed to be used in a server."
        return Nothing
      MemberOrUser (Left gm) ->
        case memberUser gm of
          Nothing -> do
            Logging.log Error $
              "Could not obtain the user data of a user that rolled a die."
            return Nothing
          Just u -> return (Just u)

    rollResponse :: Integer -> Maybe Integer -> Interaction -> DiscordHandler Integer
    rollResponse n mCall intr = do
      rand <- liftIO $ uniformRM (1, n) globalStdGen :: DiscordHandler Integer
      let response = case mCall of
            Nothing -> "You rolled a " <> showT rand <> " on the " <> "d" <> showT n <> "."
            Just c -> "You called a " <> showT c <> " on the " <> "d" <> showT n <> ", and you rolled a " <>
                      showT rand <> "."
      res <- restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic response)
      case res of
        Left err ->
          Logging.log Error $
            "The REST call to send a roll response failed. Discord returned the error:\n" <> showT err
        Right _ -> return ()

      return rand

    winCondition n mCall dieRoll = case mCall of
      Nothing -> n >= minRoll && dieRoll == n
      Just c -> n >= minRoll && dieRoll == c

    computePayout n = fromInteger (n `div` 20)

    onWin intr uid n mCall = do
      let iTok = interactionToken intr
      maybeMsg <- restCall $ -- get the message the bot responded with.
        R.GetOriginalInteractionResponse
        (interactionApplicationId intr)
        iTok
      case maybeMsg of
        Left err ->
          Logging.log Error $
            "A REST call failed when trying to get the bot's reply message to the `" <> showT cmdName <> "` command.\
            \Discord sent the following error message:\n" <> showT err
        Right msg -> do
          -- react with some rare emojis
          reacts <- liftIO $ selectNRandomRareEmojis (computePayout n) (cfgRareEmoji cfg)
          forM_ reacts $ \r -> do
            rareEmojiHandler r msg
            threadDelay 250000 -- wait 0.25 seconds in between reacts

          let userPing = "<@" <> showT uid <> ">"
              eUpdate = EmojiUpdate uid [(rareEmojiId re, 1) | re <- reacts ]
              cid = showT . messageChannelId $ msg
              mid = showT . messageId $ msg
              link = "https://discord.com/channels/" <> (showT . cfgGuildId $ cfg) <> "/" <> cid <> "/" <> mid
              method = case mCall of
                Nothing -> " rolled a " <> showT n <> " on the d" <> showT n <> "."
                Just c -> " called a " <> showT c <> " on the d" <> showT n <> ", then rolled a " <> showT c <> "."
              message = "HOLY. " <> userPing <> "just " <> method <> " CHECK IT: " <> link
          liftIO $ updateEmojis (envDBConnection env) eUpdate
          announce (cfgAnnouncementCID cfg) message

