{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
{- HLint ignore "Use newtype instead of data" -}

module Vesbot.BigRoll
( Expr(..)
, GroupedExpr(..)
, UnaryExpr (..)
, BinExpr (..)
, DivExpr (..)
, RepeatExpr (..)
, DieExpr (..)
, ConstExpr (..)
, RolledExpr(..)
, RolledGroupedExpr(..)
, RolledUnaryExpr (..)
, RolledBinExpr (..)
, RolledDivExpr (..)
, RolledRepeatExpr (..)
, RolledDieExpr (..)
, RolledConstExpr (..)
, roll
, eval
, preciseExpr
, preciseGroupedExpr
, preciseUnaryExpr
, preciseBinExpr
, preciseDivExpr
, preciseRepeatExpr
, preciseDieExpr
, preciseConstExpr
, prettyExpr
, prettyGroupedExpr
, prettyUnaryExpr
, prettyBinExpr
, prettyDivExpr
, prettyRepeatExpr
, prettyDieExpr
, prettyConstExpr
, pretty
, prettyGrouped
, prettyUnary
, prettyBin
, prettyDiv
, prettyRepeat
, prettyDie
, prettyConst
, parse
) where

import System.Random (SplitGen, uniformR, UniformRange)

import Data.Tuple.Extra (first)

import Data.Void
import qualified Data.ByteString.Char8 as BS

import Text.Megaparsec ((<?>), try)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P
import qualified Text.Megaparsec.Byte.Lexer as P.Lex
import qualified Data.List as List (intersperse)

import Control.Monad (when)
import Data.Functor (($>))
import Control.Applicative ((<|>))

type Parser = P.Parsec Void BS.ByteString


data Expr a
    = GroupedExpr (GroupedExpr a)
    | UnaryExpr   (UnaryExpr   a)
    | BinExpr     (BinExpr     a)
    | DivExpr     (DivExpr     a)
    | RepeatExpr  (RepeatExpr  a)
    | DieExpr     (DieExpr     a)
    | ConstExpr   (ConstExpr   a)
 deriving (Show, Eq)

data GroupedExpr a = Grouped        (Expr a)               deriving (Show, Eq)
data UnaryExpr   a = Unary  UnaryOp (Expr a)               deriving (Show, Eq)
data BinExpr     a = Bin    BinOp   (Expr a) (     Expr a) deriving (Show, Eq)
data DivExpr     a = Div            (Expr a) (ConstExpr a) deriving (Show, Eq)
data RepeatExpr  a = Repeat         (Expr a) (     Expr a) deriving (Show, Eq)
data DieExpr     a = Die                  a                deriving (Show, Eq)
data ConstExpr   a = Const                a                deriving (Show, Eq)

data UnaryOp = Neg           deriving (Show, Eq)
data BinOp = Add | Sub | Mul deriving (Show, Eq)

binOpPrecedence :: BinOp -> Int
binOpPrecedence Add = 5
binOpPrecedence Sub = 5
binOpPrecedence Mul = 8
divOpPrecedence :: Int
divOpPrecedence = 8

data RolledExpr a
    = RolledGroupedExpr (RolledGroupedExpr a)
    | RolledUnaryExpr   (RolledUnaryExpr a)
    | RolledBinExpr     (RolledBinExpr a)
    | RolledDivExpr     (RolledDivExpr a)
    | RolledRepeatExpr  (RolledRepeatExpr a)
    | RolledDieExpr     (RolledDieExpr a)
    | RolledConstExpr   (RolledConstExpr a)
 deriving (Show, Eq)

data RolledGroupedExpr a = RolledGrouped         (RolledExpr a)                     deriving (Show, Eq)
data RolledUnaryExpr   a = RolledUnary   UnaryOp (RolledExpr a)                     deriving (Show, Eq)
data RolledBinExpr     a = RolledBin     BinOp   (RolledExpr a) (RolledExpr      a) deriving (Show, Eq)
data RolledDivExpr     a = RolledDiv             (RolledExpr a) (RolledConstExpr a) deriving (Show, Eq)
data RolledRepeatExpr  a = RolledRepeat          [RolledExpr a]                     deriving (Show, Eq)
data RolledDieExpr     a = RolledDie                         a                      deriving (Show, Eq)
data RolledConstExpr   a = RolledConst                       a                      deriving (Show, Eq)

roll        :: (Num a, Integral a, UniformRange a) => SplitGen g => Expr        a -> g -> (RolledExpr        a, g)
rollGrouped :: (Num a, Integral a, UniformRange a) => SplitGen g => GroupedExpr a -> g -> (RolledGroupedExpr a, g)
rollUnary   :: (Num a, Integral a, UniformRange a) => SplitGen g => UnaryExpr   a -> g -> (RolledUnaryExpr   a, g)
rollBin     :: (Num a, Integral a, UniformRange a) => SplitGen g => BinExpr     a -> g -> (RolledBinExpr     a, g)
rollDiv     :: (Num a, Integral a, UniformRange a) => SplitGen g => DivExpr     a -> g -> (RolledDivExpr     a, g)
rollRepeat  :: (Num a, Integral a, UniformRange a) => SplitGen g => RepeatExpr  a -> g -> (RolledRepeatExpr  a, g)
rollDie     :: (Ord a,      Num a, UniformRange a) => SplitGen g => DieExpr     a -> g -> (RolledDieExpr     a, g)
rollConst   ::                                        SplitGen g => ConstExpr   a -> g -> (RolledConstExpr   a, g)

roll (GroupedExpr e) = first RolledGroupedExpr . rollGrouped e
roll (UnaryExpr   e) = first RolledUnaryExpr   . rollUnary   e
roll (BinExpr     e) = first RolledBinExpr     . rollBin     e
roll (DivExpr     e) = first RolledDivExpr     . rollDiv     e
roll (RepeatExpr  e) = first RolledRepeatExpr  . rollRepeat  e
roll (DieExpr     e) = first RolledDieExpr     . rollDie     e
roll (ConstExpr   e) = first RolledConstExpr   . rollConst   e

rollGrouped (Grouped e) rng = let
    (e', rng') = roll e rng
 in (RolledGrouped e', rng')
rollUnary (Unary op e) rng = let
    (e', rng') = roll e rng
 in (RolledUnary op e', rng')
rollBin (Bin op left right) rng = let
    (left',  rng' ) = roll left  rng
    (right', rng'') = roll right rng'
 in (RolledBin op left' right', rng'')
rollDiv (Div left right) rng = let
    (left',  rng' ) = roll      left  rng
    (right', rng'') = rollConst right rng'
 in (RolledDiv left' right', rng'')
rollRepeat (Repeat repeatExpr exprToRepeat) rng = let
    (repeatCount, postRepeatRng) = roll repeatExpr rng
    repeatedExprs = replicate (fromIntegral (eval repeatCount)) exprToRepeat
    (rolled, rng') = foldr rollSubExpr ([], postRepeatRng) repeatedExprs
    rollSubExpr subExpr (rolledExprs, preElementRng) =
        let (rolledExpr, postElementRng) = roll subExpr preElementRng
         in (rolledExpr:rolledExprs, postElementRng)
 in (RolledRepeat rolled, rng')
rollDie (Die n) rng = let
    range      = if n < 0 then (n, -1) else (1, n)
    (n', rng') = uniformR range rng
 in (RolledDie n', rng')
rollConst (Const n) = (RolledConst n,)

eval        :: (Num a, Integral a) => RolledExpr        a -> a
evalGrouped :: (Num a, Integral a) => RolledGroupedExpr a -> a
evalUnary   :: (Num a, Integral a) => RolledUnaryExpr   a -> a
evalBin     :: (Num a, Integral a) => RolledBinExpr     a -> a
evalDiv     :: (Num a, Integral a) => RolledDivExpr     a -> a
evalRepeat  :: (Num a, Integral a) => RolledRepeatExpr  a -> a
evalDie     ::                        RolledDieExpr     a -> a
evalConst   ::                        RolledConstExpr   a -> a

eval (RolledGroupedExpr e) = evalGrouped e
eval (RolledUnaryExpr   e) = evalUnary   e
eval (RolledBinExpr     e) = evalBin     e
eval (RolledDivExpr     e) = evalDiv     e
eval (RolledRepeatExpr  e) = evalRepeat  e
eval (RolledDieExpr     e) = evalDie     e
eval (RolledConstExpr   e) = evalConst   e

evalGrouped (RolledGrouped e) = eval e
evalUnary (RolledUnary Neg e) = -(eval e)
evalBin (RolledBin Add l r) = eval l + eval r
evalBin (RolledBin Sub l r) = eval l - eval r
evalBin (RolledBin Mul l r) = eval l * eval r
evalDiv (RolledDiv l r) = eval l `div` evalConst r
evalRepeat (RolledRepeat es) = sum . map eval $ es
evalDie (RolledDie n) = n
evalConst (RolledConst n) = n

pretty        :: (Show a) => RolledExpr        a -> String
prettyGrouped :: (Show a) => RolledGroupedExpr a -> String
prettyUnary   :: (Show a) => RolledUnaryExpr   a -> String
prettyBin     :: (Show a) => RolledBinExpr     a -> String
prettyDiv     :: (Show a) => RolledDivExpr     a -> String
prettyRepeat  :: (Show a) => RolledRepeatExpr  a -> String
prettyDie     :: (Show a) => RolledDieExpr     a -> String
prettyConst   :: (Show a) => RolledConstExpr   a -> String
prettyUnaryOp ::             UnaryOp             -> String
prettyBinOp   ::             BinOp               -> String

pretty (RolledGroupedExpr e) = prettyGrouped e
pretty (RolledUnaryExpr   e) = prettyUnary   e
pretty (RolledBinExpr     e) = prettyBin     e
pretty (RolledDivExpr     e) = prettyDiv     e
pretty (RolledRepeatExpr  e) = prettyRepeat  e
pretty (RolledDieExpr     e) = prettyDie     e
pretty (RolledConstExpr   e) = prettyConst   e

prettyGrouped (RolledGrouped  e  ) = "(" <> pretty e <> ")"
prettyUnary   (RolledUnary op e  ) = prettyUnaryOp op <> pretty e
prettyBin     (RolledBin   op l r) = unwords [ pretty l, prettyBinOp op, pretty r ]
prettyDiv     (RolledDiv      l r) = unwords [ pretty l, "/", prettyConst r ]
prettyRepeat  (RolledRepeat   es ) = unwords $ ["("] ++ (List.intersperse "+" . map pretty) es ++ [")"]
prettyDie     (RolledDie      n  ) = "**" <> show n <> "**"
prettyConst   (RolledConst    n  ) = show n
prettyUnaryOp  Neg = "-"
prettyBinOp    Add = "+"
prettyBinOp    Sub = "-"
prettyBinOp    Mul = "*"

prettyExpr         :: (Show a) => Expr        a -> String
prettyDieExpr      :: (Show a) => DieExpr     a -> String
prettyConstExpr    :: (Show a) => ConstExpr   a -> String
prettyGroupedExpr  :: (Show a) => GroupedExpr a -> String
prettyUnaryExpr    :: (Show a) => UnaryExpr   a -> String
prettyBinExpr      :: (Show a) => BinExpr     a -> String
prettyDivExpr      :: (Show a) => DivExpr     a -> String
prettyRepeatExpr   :: (Show a) => RepeatExpr  a -> String

prettyExpr (GroupedExpr e') = prettyGroupedExpr e'
prettyExpr (UnaryExpr   e') = prettyUnaryExpr   e'
prettyExpr (BinExpr     e') = prettyBinExpr     e'
prettyExpr (DivExpr     e') = prettyDivExpr     e'
prettyExpr (RepeatExpr  e') = prettyRepeatExpr  e'
prettyExpr (DieExpr     e') = prettyDieExpr     e'
prettyExpr (ConstExpr   e') = prettyConstExpr   e'

prettyGroupedExpr (Grouped    e  ) = "(" <> prettyExpr e <> ")"
prettyUnaryExpr   (Unary   op e  ) = prettyUnaryOp op <> prettyExpr e
prettyBinExpr     (Bin     op l r) = unwords [prettyExpr l, prettyBinOp op, prettyExpr r]
prettyDivExpr     (Div        l r) = unwords [prettyExpr l, "/", prettyConstExpr r]
prettyDieExpr     (Die        n  ) = "d" <> show n
prettyConstExpr   (Const      n  ) = show n
prettyRepeatExpr  (Repeat     l r) = prettyExpr l <> sep <> prettyExpr r
  where sep = case r of DieExpr _                         -> ""
                        RepeatExpr (Repeat (DieExpr _) _) -> ""
                        _                                 -> " "

preciseExpr         :: (Show a) => Expr        a -> String
preciseGroupedExpr  :: (Show a) => GroupedExpr a -> String
preciseUnaryExpr    :: (Show a) => UnaryExpr   a -> String
preciseBinExpr      :: (Show a) => BinExpr     a -> String
preciseDivExpr      :: (Show a) => DivExpr     a -> String
preciseRepeatExpr   :: (Show a) => RepeatExpr  a -> String
preciseDieExpr      :: (Show a) => DieExpr     a -> String
preciseConstExpr    :: (Show a) => ConstExpr   a -> String

preciseExpr e = unwords ["(", preciseExpr' e, ")"]
  where
    preciseExpr' (GroupedExpr e') = preciseGroupedExpr e'
    preciseExpr' (UnaryExpr   e') = preciseUnaryExpr   e'
    preciseExpr' (BinExpr     e') = preciseBinExpr     e'
    preciseExpr' (DivExpr     e') = preciseDivExpr     e'
    preciseExpr' (RepeatExpr  e') = preciseRepeatExpr  e'
    preciseExpr' (DieExpr     e') = preciseDieExpr     e'
    preciseExpr' (ConstExpr   e') = preciseConstExpr   e'

preciseGroupedExpr (Grouped    e  ) = unwords [ "(", preciseExpr e, ")" ]
preciseUnaryExpr   (Unary   op e  ) = prettyUnaryOp op <> preciseExpr e
preciseBinExpr     (Bin     op l r) = unwords [preciseExpr l, prettyBinOp op, preciseExpr r]
preciseDivExpr     (Div        l r) = unwords [preciseExpr l, "/", preciseConstExpr r]
preciseRepeatExpr  (Repeat     l r) = unwords [preciseExpr l, preciseExpr r]
preciseDieExpr     (Die        n  ) = "d" <> show n
preciseConstExpr   (Const      n  ) = show n


{-
    S      -> expr EOF
    expr   -> left right*
    left   -> "(" expr ")" | const | die | unary
    unary  -> unaryOp left
    die    -> "d" decimal | "D" decimal
    const  -> "-"? decimal
    right  -> binary | div | repeat
    binary -> binaryOp expr
    div    -> "/" nzonst
    repeat -> left | left
-}

parse :: (Show a, Num a, Integral a) =>  Parser (Expr a)
parse = P.space *> pExpr 0 <* P.eof

pExpr :: (Show a, Num a, Integral a) =>  Int -> Parser (Expr a)
pExpr prec = do
    l <- pLeft
    P.space
    let lr = pRight prec l <* P.space
    try lr <|> return l

pLeft :: (Show a, Num a, Integral a) => Parser (Expr a)
pLeft = do
    paren <- try $ P.withRecovery
        (const $ return False)
        (P.string "(" $> True)

    if paren then do
        P.space
        e <- pExpr 0 <?> "expression"
        P.space
        _ <- P.string ")"
        return $ GroupedExpr $ Grouped e
     else
            (ConstExpr <$> pConst   <?> "constant"        ) <|>
            (DieExpr   <$> pDieExpr <?> "die expression"  ) <|>
            (UnaryExpr <$> pUnary   <?> "unary expression")

pUnary :: (Show a, Num a, Integral a) =>  Parser (UnaryExpr a)
pUnary = do
    op      <- try pUnaryOp <?> "unary operator"
    operand <- try pLeft    <?> "operand"
    return $ Unary op operand

pUnaryOp :: Parser UnaryOp
pUnaryOp = do
    Neg <$ P.string "-"

{-# ANN pDieExpr ( "HLint: ignore Use <$>" :: String ) #-}
pDieExpr :: (Num a) => Parser (DieExpr a)
pDieExpr = do
    _ <- P.string "d" <|> P.string "D"
    -- TODO: range check
    n <- P.Lex.decimal <?> "die size"
    return $ Die n

{-# ANN pConst ( "HLint: ignore Use <$>" :: String ) #-}
pConst :: (Num a) => Parser (ConstExpr a)
pConst = do
    -- TODO: range check
    n <- P.Lex.decimal
    return $ Const n


pRight :: (Show a, Num a, Integral a) => Int -> Expr a -> Parser (Expr a)
pRight prec l = do
    right <- try (BinExpr    <$> pBin    prec l ) <|>
             try (DivExpr    <$> pDiv    prec l ) <|>
             try (RepeatExpr <$> pRepeat prec l )
    P.space
    (try (pRight prec right) <?> "left-associative right") <|> return right

pBin :: (Show a, Num a, Integral a) =>  Int -> Expr a -> Parser (BinExpr a)
pBin prec l = do
    op <- try $ pBinOpWithPrecedence prec
    P.space
    r <- try (pExpr (binOpPrecedence op + 1)) <?> "right operand"
    let e = Bin op l r
    return e
        

pDiv :: (Num a, Integral a) => Int -> Expr a -> Parser (DivExpr a)
pDiv prec l = do
    _ <- P.string "/"
    _ <- checkPrecedence prec divOpPrecedence
    P.space
    let pDivisor = do
        c@(Const n) <- pConst
        when (n == 0) (fail "divisor must be nonzero")
        return c
    divisor <- try pDivisor <?> "non-zero divisor"
    return $ Div l divisor

pBinOpWithPrecedence :: Int -> Parser BinOp
pBinOpWithPrecedence prec = do
    op <- pBinOp <?> "binary operator"
    _  <- checkPrecedence prec (binOpPrecedence op)
    return op

pBinOp :: Parser BinOp
pBinOp =
    (Add <$ P.string "+") <|>
    (Sub <$ P.string "-") <|>
    (Mul <$ P.string "*")
    
checkPrecedence :: Int -> Int -> Parser Int
checkPrecedence minPrec prec = do
    when (prec < minPrec)
         (fail $ unwords ["precedence", show prec, "<", show minPrec])
    return prec

{-# ANN pRepeat ( "HLint: ignore Use <$>" :: String ) #-}
pRepeat :: (Show a, Num a, Integral a) =>  Int -> Expr a -> Parser (RepeatExpr a)
pRepeat _prec l = do
    r <- pLeft
    let e = Repeat l r
    return e

