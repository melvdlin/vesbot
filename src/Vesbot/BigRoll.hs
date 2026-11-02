{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{- HLint ignore "Use newtype instead of data" -}

module Vesbot.BigRoll (
    Expr, RolledExpr, roll, eval, parse
) where

import Control.Applicative ((<|>))

import Debug.Trace (trace)
import Data.ByteString.Internal (w2c)
import Data.Attoparsec.Combinator ((<?>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto

import System.Random (SplitGen, uniformR)

data Expr
    = ConstExpr  ConstExpr
    | DieExpr    DieExpr
    | BinExpr    BinExpr
    | DivExpr    DivExpr
    | RepeatExpr RepeatExpr
 deriving (Show, Eq)

data ConstExpr  = Const  Int             deriving (Show, Eq)
data DieExpr    = Die    Int             deriving (Show, Eq)
data BinExpr    = Bin    BinOp Expr Expr deriving (Show, Eq)
data DivExpr    = Div    Expr  ConstExpr deriving (Show, Eq)
data RepeatExpr = Repeat       Expr Expr deriving (Show, Eq)

data BinOp = Add | Sub | Mul deriving (Show, Eq)

data RolledExpr
    = RolledConstExpr  RolledConstExpr
    | RolledDieExpr    RolledDieExpr
    | RolledBinExpr    RolledBinExpr
    | RolledDivExpr    RolledDivExpr
    | RolledRepeatExpr RolledRepeatExpr
 deriving (Show, Eq)

data RolledConstExpr  = RolledConst  Int                              deriving (Show, Eq)
data RolledDieExpr    = RolledDie    Int                              deriving (Show, Eq)
data RolledBinExpr    = RolledBin    BinOp RolledExpr RolledExpr      deriving (Show, Eq)
data RolledDivExpr    = RolledDiv          RolledExpr RolledConstExpr deriving (Show, Eq)
data RolledRepeatExpr = RolledRepeat [RolledExpr]                     deriving (Show, Eq)

roll :: SplitGen g => Expr -> g -> (RolledExpr, g)
roll (ConstExpr  e) = mapFst RolledConstExpr  . rollConst  e
roll (DieExpr    e) = mapFst RolledDieExpr    . rollDie    e
roll (BinExpr    e) = mapFst RolledBinExpr    . rollBin    e
roll (DivExpr    e) = mapFst RolledDivExpr    . rollDiv    e
roll (RepeatExpr e) = mapFst RolledRepeatExpr . rollRepeat e

rollConst :: SplitGen g => ConstExpr -> g -> (RolledConstExpr, g)
rollConst (Const n) = (RolledConst n,)
rollDie :: SplitGen g => DieExpr -> g -> (RolledDieExpr, g)
rollDie (Die n) rng = let
    range      = if n < 0 then (n, 0) else (0, n)
    (n', rng') = uniformR range rng
 in (RolledDie n', rng')
rollBin :: SplitGen g => BinExpr -> g -> (RolledBinExpr, g)
rollBin (Bin op left right) rng = let
    (left',  rng' ) = roll left  rng
    (right', rng'') = roll right rng'
 in (RolledBin op left' right', rng'')
rollDiv :: SplitGen g => DivExpr -> g -> (RolledDivExpr, g)
rollDiv (Div left right) rng = let
    (left',  rng' ) = roll      left  rng
    (right', rng'') = rollConst right rng'
 in (RolledDiv left' right', rng'')
rollRepeat :: SplitGen g => RepeatExpr -> g -> (RolledRepeatExpr, g)
rollRepeat (Repeat repeatExpr exprToRepeat) rng = let
    (repeatCount, postRepeatRng) = roll repeatExpr rng
    repeatedExprs = replicate (eval repeatCount) exprToRepeat
    (rolled, rng') = foldr rollSubExpr ([], postRepeatRng) repeatedExprs
    rollSubExpr subExpr (rolledExprs, preElementRng) =
        let (rolledExpr, postElementRng) = roll subExpr preElementRng
         in (rolledExpr:rolledExprs, postElementRng)
 in (RolledRepeat rolled, rng')

eval :: RolledExpr -> Int
eval (RolledConstExpr  e) = evalConst  e
eval (RolledDieExpr    e) = evalDie    e
eval (RolledBinExpr    e) = evalBin    e
eval (RolledDivExpr    e) = evalDiv    e
eval (RolledRepeatExpr e) = evalRepeat e

evalConst :: RolledConstExpr -> Int
evalConst (RolledConst n) = n
evalDie :: RolledDieExpr -> Int
evalDie (RolledDie n) = n
evalBin :: RolledBinExpr -> Int
evalBin (RolledBin Add l r) = eval l + eval r
evalBin (RolledBin Sub l r) = eval l - eval r
evalBin (RolledBin Mul l r) = eval l * eval r
evalDiv :: RolledDivExpr -> Int
evalDiv (RolledDiv l r) = eval l `div` evalConst r
evalRepeat :: RolledRepeatExpr -> Int
evalRepeat (RolledRepeat es) = sum . map eval $ es

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (l, r) = (f l, r)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (l, r) = (l, f r)

parse :: Atto.Parser Expr
parse = Atto.choice $ (<* Atto.endOfInput) <$> parseExpr

-- TODO: figure out proper backtracking

parseExpr :: [Atto.Parser Expr]
parseExpr =
    (traceParser "expr" *>) <$>
    [ ConstExpr   <$> parseConstExpr
    , DieExpr     <$> parseDieExpr
    , BinExpr     <$> parseBinExpr
    , DivExpr     <$> parseDivExpr
    , RepeatExpr  <$> parseRepeatExpr
    , Atto.choice  $ parseGrouped <$> parseExpr
    ]

-- Always consumes input or fails
parseProgressiveExpr :: [Atto.Parser Expr]
parseProgressiveExpr =
    (traceParser "progressiveExpr" *>) <$>
    [ ConstExpr   <$> parseConstExpr
    , DieExpr     <$> parseDieExpr
    , Atto.choice  $ parseGrouped <$> parseExpr
    ] 

parseGrouped :: Atto.Parser a -> Atto.Parser a
parseGrouped p = 
    traceParser "grouped" *> 
       (Atto.string "(" <?> "(")
    *> Atto.skipSpace
    *> p
    <* traceParser "post-nest"
    <* Atto.skipSpace
    <* (Atto.string ")" <?> ")")
    <?> "grouped expression"

parseConstExpr :: Atto.Parser ConstExpr
parseConstExpr = (traceParser "constExpr" *>) $ Const <$> parseConst

parseNonZeroConstExpr :: Atto.Parser ConstExpr
parseNonZeroConstExpr = (traceParser "nonZeroConstExpr" *>) $ Const <$> parseNonZeroConst

{-# ANN parseBinExpr ( "HLint: ignore Use <$>" :: String ) #-}
parseBinExpr :: Atto.Parser BinExpr
parseBinExpr = (traceParser "binExpr" *>) $  (<?> "binary expression") $ do
    left  <- Atto.choice parseProgressiveExpr
    Atto.skipSpace
    op    <- parseBinOp
    Atto.skipSpace
    right <- Atto.choice parseExpr
    return $ Bin op left right

parseBinOp :: Atto.Parser BinOp
parseBinOp =
    traceParser "binOp" *>
    (Add <$ Atto.string "+") <|>
    (Sub <$ Atto.string "-") <|>
    (Mul <$ Atto.string "*") <?>
    "binary operator"

{-# ANN parseDivExpr ( "HLint: ignore Use <$>" :: String ) #-}
parseDivExpr :: Atto.Parser DivExpr
parseDivExpr = (traceParser "divExpr" *>) $  (<?> "division expression") $ do
    left <- Atto.choice parseProgressiveExpr
    Atto.skipSpace
    _ <- Atto.string "/"
    Atto.skipSpace
    right <- parseNonZeroConstExpr
    return $ Div left right

{-# ANN parseRepeatExpr ( "HLint: ignore Use <$>" :: String ) #-}
parseRepeatExpr :: Atto.Parser RepeatExpr
parseRepeatExpr = (traceParser "repeatExpr" *>) $ (<?> "repeat expression") $ do
    left  <- Atto.choice parseProgressiveExpr
    Atto.skipSpace
    right <- Atto.choice parseExpr
    return $ Repeat left right

parseNonZeroConst :: Atto.Parser Int
parseNonZeroConst = (<?> "non-zero constant") $ do
    n <- parseConst
    if n == 0
      then fail "constant is zero"
      else return n

parseConst :: Atto.Parser Int
parseConst = (<?> "constant") $ do
    sign <- (-1 <$ Atto.string "-") <|> return 1
    n    <- Atto.decimal
    return $ sign * n

parseDieExpr :: Atto.Parser DieExpr
parseDieExpr = traceParser "dieExpr" *>
    ((Atto.string "d" <|> Atto.string "D") <?> "d") *>
    (Die <$> parseConst) <?>
    "die"

traceParser :: String -> Atto.Parser ()
traceParser tag = do
    next <- Atto.peekWord8
    trace (tag <> ": " <> show (w2c <$> next)) (return ())  