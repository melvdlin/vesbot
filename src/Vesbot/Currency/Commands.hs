{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Vesbot.Currency.Commands
  ( payCommand
  , viewCurrencyCommand
  , viewRichestUsersCommand
  , viewEmojisCommand
  , showC
  ) where

import Vesbot.Utils (showT, maybeToEither, ephemeralResponseBasic, liftIO, void)
import Vesbot.Parsing (getUserData, extractUserOption, extractNumberOption, extractField, extractOptionsData)
import Vesbot.Config (Config(..), CurrencyConfig(..))
import Vesbot.Database.Types (DBConnection)
import Vesbot.Currency.Types (PayCommandData(..), CurrencyQuery(..), EmojiQuery(..), CurrencyUpdate(..))
import Vesbot.Currency.Database (getCurrency, getEmojis, updateCurrency, getRichestUsers)
import Vesbot.BotCommands.Types (BotCommand(..))

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.Interactions

import qualified Data.Text as T


splitDouble :: Double -> (Int, Int, Int)
splitDouble d =
    let millis = round $ d * (10^(3 :: Int))
        (kilo, rest1)   = divMod millis (10^(6 :: Int))
        (normal, milli) = divMod rest1 (10^(3 :: Int))
    in (kilo, normal, milli)

showC :: CurrencyConfig -> Double -> T.Text
showC (CurrencyConfig _ sk sn sm) d =
  let (k, n, m) = splitDouble d
      noZeros =
        map (\(units,v) -> units <> showT v)
        . filter (\(_,v) -> v /= 0)
        $ [(sk, k), (sn, n), (sm, m)]
  in
    T.intercalate " " $ if null noZeros then [sm <> "0"] else noZeros

getUser :: Interaction -> Maybe UserId
getUser (InteractionComponent                      { interactionUser }) = getUserData interactionUser  
getUser (InteractionPing                           {                 }) = Nothing  
getUser (InteractionApplicationCommand             { interactionUser }) = getUserData interactionUser  
getUser (InteractionApplicationCommandAutocomplete { interactionUser }) = getUserData interactionUser  
getUser (InteractionModalSubmit                    { interactionUser }) = getUserData interactionUser  

parseUserFieldCommand :: T.Text -> Interaction -> Maybe OptionsData -> Either T.Text UserId
parseUserFieldCommand cmdName (InteractionApplicationCommand {}) opts = do
  optValues <- extractOptionsData cmdName opts
  extractField extractUserOption "user" cmdName optValues

parseUserFieldCommand _ _ _ = Left "Tried to parse a user field in a non application command interaction."


parsePayCommand :: Interaction -> Maybe OptionsData -> Either T.Text PayCommandData
parsePayCommand
  InteractionApplicationCommand
  { applicationCommandData = cmdData
  , interactionUser
  }
  opts = do
  let cmdName = applicationCommandDataName cmdData

  optValues <- extractOptionsData cmdName opts
  payTo  <- extractField extractUserOption "pay_to" cmdName optValues
  amount <- extractField extractNumberOption "amount" cmdName optValues
  uid    <- maybeToEither
      "Could not get user data. Note that this command can only be used in a server."
      (getUserData interactionUser)
  
  return $
    PayCommandData
      { payFromUser = uid
      , payToUser = payTo
      , payAmount = amount }

parsePayCommand _ _ = Left "Tried to parse a pay command on an interaction that is not an application command."


createPaymentResponse :: DBConnection -> CurrencyConfig -> Either T.Text PayCommandData -> IO InteractionResponse
createPaymentResponse dbconn ccfg pcd =
  let cn = currencyName ccfg
  in  case pcd of
        Left err -> return (ephemeralResponseBasic err)
        Right p -> do
          CurrencyQuery { currencyQueryAmount = userCurrency } <- getCurrency dbconn (payFromUser p)
          response <- do
            if userCurrency >= payAmount p then
              let response =
                    "Paid " <> showC ccfg (payAmount p) <> " to <@" <> showT (payToUser p) <> ">."
                  remove = CurrencyUpdate (payFromUser p) ((-1) * payAmount p)
                  add = CurrencyUpdate (payToUser p) (payAmount p)
              in do
                  updateCurrency dbconn remove -- TODO: can be slightly more efficient as a single SQL transaction...
                  updateCurrency dbconn add
                  return response
              else do
                return $ "You don't have enough " <> cn <> "!"
          return (ephemeralResponseBasic response)


createViewCurrencyResponse :: DBConnection -> CurrencyConfig -> Either T.Text UserId -> IO InteractionResponse
createViewCurrencyResponse dbconn ccfg userOrErr =
  case userOrErr of
    Left err -> return (ephemeralResponseBasic err)
    Right uid -> do
      c <- getCurrency dbconn uid
      let userPing = "<@" <> showT uid <> ">"
          response = userPing <> " has " <> showC ccfg (currencyQueryAmount c) <> "."
      return (ephemeralResponseBasic response)


createViewEmojisResponse :: DBConnection -> Either T.Text UserId -> IO InteractionResponse
createViewEmojisResponse dbconn userOrErr = do
  case userOrErr of
    Left err -> return (ephemeralResponseBasic err)
    Right uid -> do
      res <- getEmojis dbconn uid
      let as = emojiQueryAmounts res
          userPing = "<@" <> showT uid <> ">"
          response = if null as
            then userPing <> " has no emojis!"
            else userPing <> "'s rare emoji inventory:\n" <> T.intercalate "  |  " [ "<:" <> name <> ">" <> ": " <> showT amt | (name, amt) <- as, amt /= 0 ]
      return (ephemeralResponseBasic response)


createViewRichestUsersResponse :: DBConnection -> CurrencyConfig -> IO InteractionResponse
createViewRichestUsersResponse dbconn ccfg = do
  qs <- getRichestUsers dbconn
  let rs =
        flip map (zip [1::Int ..] qs) $ \(n, q) ->
          let userPing = "<@" <> showT (currencyQueryUser q) <> ">"
              r = showT n <> ". " <> userPing <> ": " <> showC ccfg (currencyQueryAmount q) <> "."
          in r
      response = T.intercalate "\n" rs :: T.Text
      pingPerms = AllowedMentions False False False [] [] True
      message = InteractionResponseMessage (Just False) (Just response) Nothing (Just pingPerms) Nothing Nothing Nothing []
  return (InteractionResponseChannelMessage message)
  


payCommand :: DBConnection -> Config -> BotCommand
payCommand dbconn cfg = SlashCommand
  { commandName = "pay"
  , commandRegistration = Just reg
  , slashCommandHandler = \intr opts -> do
      let pcd = parsePayCommand intr opts :: Either T.Text PayCommandData
      botReply <- liftIO $ createPaymentResponse dbconn (cfgCurrencyInfo cfg) pcd
      -- Respond with the error or the correct reply
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          botReply

      -- TODO: Database stuff
  }
    where
      cn = currencyName . cfgCurrencyInfo $ cfg
      o1 = OptionValueUser "pay_to" Nothing ("Who to send the " <> cn <> " to") Nothing True
      o2 = OptionValueNumber "amount" Nothing "Amount to send" Nothing True (Left False) (Just 0.001) Nothing
      reg = -- Command registration
        CreateApplicationCommandChatInput
          "pay"
          Nothing
          ("Send " <> cn <> " to a user")
          Nothing 
          (Just $ OptionsValues [o1, o2])
          Nothing
          (Just False)


viewCurrencyCommand :: DBConnection -> Config -> BotCommand
viewCurrencyCommand dbconn cfg = SlashCommand
  { commandName = "viewcurrency"
  , commandRegistration = Just reg
  , slashCommandHandler = \intr opts -> do
      let uid = parseUserFieldCommand "viewcurrency" intr opts
      let self = maybeToEither "viewcurrency: failed to get user" $ getUser intr 
      let user = either (const self) Right uid
      botReply <- liftIO $ createViewCurrencyResponse dbconn (cfgCurrencyInfo cfg) user
      -- Respond with the error or the correct reply
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          botReply
  }
    where
      cn = currencyName . cfgCurrencyInfo $ cfg
      o1 = OptionValueUser "user" Nothing "User to check. Defaults to self." Nothing False
      reg = -- Command registration
        CreateApplicationCommandChatInput
          "viewcurrency"
          Nothing
          ("Check how many " <> cn <> " someone has")
          Nothing
          (Just $ OptionsValues [ o1 ])
          Nothing
          (Just False)


viewRichestUsersCommand :: DBConnection -> Config -> BotCommand
viewRichestUsersCommand dbconn cfg =
  let cn = currencyName . cfgCurrencyInfo $ cfg
  in  SlashCommand
        { commandName = "rich"
        , commandRegistration = createChatInput "rich" ("View the users with the most " <> cn)
        , slashCommandHandler = \intr _ -> do
            botReply <- liftIO $ createViewRichestUsersResponse dbconn (cfgCurrencyInfo cfg)
            void . restCall $
              R.CreateInteractionResponse
                (interactionId intr)
                (interactionToken intr)
                botReply
        }


viewEmojisCommand :: DBConnection -> BotCommand
viewEmojisCommand dbconn = SlashCommand
  { commandName = "viewemojis"
  , commandRegistration = Just reg
  , slashCommandHandler = \intr opts -> do
      let uid = parseUserFieldCommand "viewemojis" intr opts
      let self = maybeToEither "viewemojis: failed to get user" $ getUser intr 
      let user = either (const self) Right uid
      botReply <- liftIO $ createViewEmojisResponse dbconn user
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          botReply 
  }
    where
      o1 = OptionValueUser "user" Nothing "User to check. Defaults to self." Nothing False
      reg = -- Command registration
        CreateApplicationCommandChatInput
          "viewemojis"
          Nothing
          "Check how many emojis someone has"
          Nothing
          (Just $ OptionsValues [ o1 ])
          Nothing
          (Just False)
