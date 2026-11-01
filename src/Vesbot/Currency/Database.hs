{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Vesbot.Currency.Database where

import Vesbot.Utils (showT, forM_)
import Vesbot.Database.Types (DBConnection, withDB)
import Vesbot.Currency.Types (CurrencyQuery(..), CurrencyUpdate(..), EmojiQuery(..), EmojiUpdate(..))

import Discord.Types

import Database.SQLite.Simple as SQL
import qualified Data.Text as T
import Data.Int (Int64)

initCurrencyTable :: DBConnection -> IO ()
initCurrencyTable dbconn = withDB dbconn $ \conn ->
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS currency(\
    \  user_id       TEXT NOT NULL PRIMARY KEY,\
    \  currency_amt  REAL NOT NULL DEFAULT 0) STRICT"

initEmojiTable :: DBConnection -> IO ()
initEmojiTable dbconn = withDB dbconn $ \conn ->
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS emoji_currency(\
    \  user_id        TEXT NOT NULL,\
    \  emoji_id       TEXT NOT NULL,\
    \  emoji_amt      INTEGER NOT NULL DEFAULT 0,\
    \  PRIMARY KEY (user_id, emoji_id)) WITHOUT ROWID, STRICT"

getEmojis :: DBConnection -> UserId -> IO EmojiQuery
getEmojis dbconn uid = withDB dbconn $
  \conn -> do
    res <- SQL.query conn
      "SELECT emoji_id, emoji_amt FROM emoji_currency \
      \WHERE user_id = ?"
      [showT uid] :: IO [(T.Text, Int64)]
    return $ EmojiQuery uid res

updateEmojis :: DBConnection -> EmojiUpdate -> IO ()
updateEmojis dbconn eu = withDB dbconn $
  \conn ->
    SQL.withTransaction conn $ do
      forM_ (emojiUpdateAmounts eu) $ \(eid, change) -> oneUpdate conn eid change
  where
    oneUpdate conn eid change =
      SQL.executeNamed conn
        "INSERT INTO emoji_currency (user_id, emoji_id, emoji_amt) VALUES (:userid, :eid, :change) \
        \ON CONFLICT (user_id, emoji_id) \
        \DO UPDATE SET emoji_amt = emoji_amt + :change"
        [ ":userid" SQL.:= (showT . emojiUpdateUser $ eu), ":eid" SQL.:= eid, ":change" SQL.:= change ]

getCurrency :: DBConnection -> UserId -> IO CurrencyQuery
getCurrency dbconn uid = withDB dbconn $
  \conn -> do
    res <- SQL.query conn
      "SELECT currency_amt FROM currency WHERE user_id = ?"
      [showT uid] :: IO [SQL.Only (Maybe Double)]
    case res of
      [SQL.Only (Just amt)] -> return (CurrencyQuery uid amt)
      _                     -> return (CurrencyQuery uid 0)

updateCurrency :: DBConnection -> CurrencyUpdate -> IO ()
updateCurrency dbconn cu = withDB dbconn $
  \conn -> do
    SQL.executeNamed conn
      "INSERT INTO currency (user_id, currency_amt) VALUES (:userid, :change)\
      \ON CONFLICT(user_id)\
      \DO UPDATE SET currency_amt = currency_amt + :change"
      [ ":change" SQL.:= currencyUpdateAmount cu, ":userid" SQL.:= (showT . currencyUpdateUser $ cu) ]

getRichestUsers :: DBConnection -> IO [CurrencyQuery]
getRichestUsers dbconn = withDB dbconn $
  \conn -> SQL.query_ conn
    "SELECT user_id, currency_amt FROM currency \
    \ORDER BY currency_amt DESC \
    \LIMIT 10"
