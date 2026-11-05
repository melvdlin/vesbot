{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
{- HLint ignore "Use newtype instead of data" -}

module Vesbot.BigRoll (
    Expr, RolledExpr, roll, eval, prettyExpr, pretty, parse
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
    = UnaryExpr  (UnaryExpr  a)
    | BinExpr    (BinExpr    a)
    | DivExpr    (DivExpr    a)
    | RepeatExpr (RepeatExpr a)
    | DieExpr    (DieExpr    a)
    | ConstExpr  (ConstExpr  a)
 deriving (Show, Eq)

data UnaryExpr  a = Unary  UnaryOp (Expr a)               deriving (Show, Eq)
data BinExpr    a = Bin    BinOp   (Expr a) (     Expr a) deriving (Show, Eq)
data DivExpr    a = Div            (Expr a) (ConstExpr a) deriving (Show, Eq)
data RepeatExpr a = Repeat         (Expr a) (     Expr a) deriving (Show, Eq)
data DieExpr    a = Die                  a                deriving (Show, Eq)
data ConstExpr  a = Const                a                deriving (Show, Eq)

data UnaryOp = Neg           deriving (Show, Eq)
data BinOp = Add | Sub | Mul deriving (Show, Eq)

binOpPrecedence :: BinOp -> Int
binOpPrecedence Add = 5
binOpPrecedence Sub = 5
binOpPrecedence Mul = 8
divOpPrecedence :: Int
divOpPrecedence = 8

data RolledExpr a
    = RolledUnaryExpr  (RolledUnaryExpr a)
    | RolledBinExpr    (RolledBinExpr a)
    | RolledDivExpr    (RolledDivExpr a)
    | RolledRepeatExpr (RolledRepeatExpr a)
    | RolledDieExpr    (RolledDieExpr a)
    | RolledConstExpr  (RolledConstExpr a)
 deriving (Show, Eq)

data RolledUnaryExpr  a = RolledUnary  UnaryOp (RolledExpr a)                     deriving (Show, Eq)
data RolledBinExpr    a = RolledBin    BinOp   (RolledExpr a) (RolledExpr      a) deriving (Show, Eq)
data RolledDivExpr    a = RolledDiv            (RolledExpr a) (RolledConstExpr a) deriving (Show, Eq)
data RolledRepeatExpr a = RolledRepeat         [RolledExpr a]                     deriving (Show, Eq)
data RolledDieExpr    a = RolledDie                        a                      deriving (Show, Eq)
data RolledConstExpr  a = RolledConst                      a                      deriving (Show, Eq)

roll :: (Num a, Integral a, UniformRange a) => SplitGen g => Expr a -> g -> (RolledExpr a, g)
roll (UnaryExpr  e) = first RolledUnaryExpr  . rollUnary  e
roll (BinExpr    e) = first RolledBinExpr    . rollBin    e
roll (DivExpr    e) = first RolledDivExpr    . rollDiv    e
roll (RepeatExpr e) = first RolledRepeatExpr . rollRepeat e
roll (DieExpr    e) = first RolledDieExpr    . rollDie    e
roll (ConstExpr  e) = first RolledConstExpr  . rollConst  e

rollUnary :: (Num a, Integral a, UniformRange a) => SplitGen g => UnaryExpr a -> g -> (RolledUnaryExpr a, g)
rollUnary (Unary op e) rng = let
    (e', rng') = roll e rng
 in (RolledUnary op e', rng')
rollBin :: (Num a, Integral a, UniformRange a) => SplitGen g => BinExpr a -> g -> (RolledBinExpr a, g)
rollBin (Bin op left right) rng = let
    (left',  rng' ) = roll left  rng
    (right', rng'') = roll right rng'
 in (RolledBin op left' right', rng'')
rollDiv :: (Num a, Integral a, UniformRange a) =>  SplitGen g => DivExpr a -> g -> (RolledDivExpr a, g)
rollDiv (Div left right) rng = let
    (left',  rng' ) = roll      left  rng
    (right', rng'') = rollConst right rng'
 in (RolledDiv left' right', rng'')
rollRepeat :: (Num a, Integral a, UniformRange a) => SplitGen g => RepeatExpr a -> g -> (RolledRepeatExpr a, g)
rollRepeat (Repeat repeatExpr exprToRepeat) rng = let
    (repeatCount, postRepeatRng) = roll repeatExpr rng
    repeatedExprs = replicate (fromIntegral (eval repeatCount)) exprToRepeat
    (rolled, rng') = foldr rollSubExpr ([], postRepeatRng) repeatedExprs
    rollSubExpr subExpr (rolledExprs, preElementRng) =
        let (rolledExpr, postElementRng) = roll subExpr preElementRng
         in (rolledExpr:rolledExprs, postElementRng)
 in (RolledRepeat rolled, rng')
rollDie :: (Ord a, Num a, UniformRange a) => SplitGen g => DieExpr a -> g -> (RolledDieExpr a, g)
rollDie (Die n) rng = let
    range      = if n < 0 then (n, 0) else (0, n)
    (n', rng') = uniformR range rng
 in (RolledDie n', rng')
rollConst :: SplitGen g => ConstExpr a -> g -> (RolledConstExpr a, g)
rollConst (Const n) = (RolledConst n,)

eval :: (Num a, Integral a) =>  RolledExpr a -> a
eval (RolledUnaryExpr  e) = evalUnary  e
eval (RolledBinExpr    e) = evalBin    e
eval (RolledDivExpr    e) = evalDiv    e
eval (RolledRepeatExpr e) = evalRepeat e
eval (RolledDieExpr    e) = evalDie    e
eval (RolledConstExpr  e) = evalConst  e

evalUnary :: (Num a, Integral a) => RolledUnaryExpr a -> a
evalUnary (RolledUnary Neg e) = -(eval e)
evalBin :: (Num a, Integral a) => RolledBinExpr a -> a
evalBin (RolledBin Add l r) = eval l + eval r
evalBin (RolledBin Sub l r) = eval l - eval r
evalBin (RolledBin Mul l r) = eval l * eval r
evalDiv :: (Num a, Integral a) => RolledDivExpr a -> a
evalDiv (RolledDiv l r) = eval l `div` evalConst r
evalRepeat :: (Num a, Integral a) => RolledRepeatExpr a -> a
evalRepeat (RolledRepeat es) = sum . map eval $ es
evalDie :: RolledDieExpr a -> a
evalDie (RolledDie n) = n
evalConst :: RolledConstExpr a -> a
evalConst (RolledConst n) = n

pretty :: (Show a) => RolledExpr a -> String
pretty (RolledUnaryExpr  e) = prettyUnary  e
pretty (RolledBinExpr    e) = prettyBin    e
pretty (RolledDivExpr    e) = prettyDiv    e
pretty (RolledRepeatExpr e) = prettyRepeat e
pretty (RolledDieExpr    e) = prettyDie    e
pretty (RolledConstExpr  e) = prettyConst  e

prettyUnary :: (Show a) => RolledUnaryExpr a -> String
prettyUnary (RolledUnary Neg e) = "-" <> pretty e
prettyUnaryOp :: UnaryOp -> String
prettyUnaryOp Neg = "-"
prettyBin :: (Show a) => RolledBinExpr a -> String
prettyBin (RolledBin op l r) = unwords [ pretty l, prettyBinOp op, pretty r ]
prettyBinOp :: BinOp -> String
prettyBinOp Add = "+"
prettyBinOp Sub = "-"
prettyBinOp Mul = "*"
prettyDiv :: (Show a) => RolledDivExpr a -> String
prettyDiv (RolledDiv l r) = unwords [ pretty l, "/", prettyConst r ]
prettyRepeat :: (Show a) => RolledRepeatExpr a -> String
prettyRepeat (RolledRepeat es) = unwords $ ["("] ++ (List.intersperse "+" . map pretty) es ++ [")"]
prettyDie :: (Show a) => RolledDieExpr a -> String
prettyDie (RolledDie n) = "**" <> show n <> "**"
prettyConst :: (Show a) => RolledConstExpr a -> String
prettyConst (RolledConst n) = show n

prettyExpr :: (Show a) => Expr a -> String
prettyExpr e = let
    prettyExpr' (UnaryExpr  e') = prettyUnaryExpr  e'
    prettyExpr' (BinExpr    e') = prettyBinExpr    e'
    prettyExpr' (DivExpr    e') = prettyDivExpr    e'
    prettyExpr' (RepeatExpr e') = prettyRepeatExpr e'
    prettyExpr' (DieExpr    e') = prettyDieExpr    e'
    prettyExpr' (ConstExpr  e') = prettyConstExpr  e'
    in unwords ["(", prettyExpr' e, ")"]

prettyUnaryExpr  :: (Show a) => UnaryExpr a -> String
prettyUnaryExpr (Unary op e) = prettyUnaryOp op <> prettyExpr e
prettyBinExpr    :: (Show a) => BinExpr a -> String
prettyBinExpr (Bin op l r) = unwords [prettyExpr l, prettyBinOp op, prettyExpr r]
prettyDivExpr    :: (Show a) => DivExpr a -> String
prettyDivExpr (Div l r) = unwords [prettyExpr l, "/", prettyConstExpr r]
prettyRepeatExpr :: (Show a) => RepeatExpr a -> String
prettyRepeatExpr (Repeat l r) = unwords [prettyExpr l, prettyExpr r]
prettyDieExpr    :: (Show a) => DieExpr a -> String
prettyDieExpr (Die n) = "d" <> show n
prettyConstExpr  :: (Show a) => ConstExpr a -> String
prettyConstExpr (Const n) = show n


{-
    S      -> expr EOF
    expr   -> left right?
    left   -> "(" expr ")" | const | die | unary
    unary  -> unaryOp left
    die    -> "d" decimal | "D" decimal
    const  -> "-"? decimal
    right  -> binary | div | repeat
    div    -> "/" nzonst
    binary -> binaryOp expr
    repeat    -> expr
-}

parse :: (Num a, Integral a) =>  Parser (Expr a)
parse = P.space *> pExpr 0 <* P.eof

pExpr :: (Num a, Integral a) =>  Int -> Parser (Expr a)
pExpr prec = do
    l <- pLeft
    P.space
    let lr = pRight prec l <* P.space
    try lr <|> return l

pLeft :: (Num a, Integral a) => Parser (Expr a)
pLeft = do
    paren <- try $ P.withRecovery
        (const $ return False)
        (P.string "(" $> True)

    if paren then do
        P.space
        e <- pExpr 0 <?> "expression"
        P.space
        _ <- P.string ")"
        return e
     else
            (ConstExpr <$> pConst   <?> "constant"        ) <|>
            (DieExpr   <$> pDieExpr <?> "die expression"  ) <|>
            (UnaryExpr <$> pUnary   <?> "unary expression")

pUnary :: (Num a, Integral a) =>  Parser (UnaryExpr a)
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


pRight :: (Num a, Integral a) => Int -> Expr a -> Parser (Expr a)
pRight prec l =
    try (BinExpr    <$> pBin    prec l ) <|>
    try (DivExpr    <$> pDiv    prec l ) <|>
    try (RepeatExpr <$> pRepeat prec l )

pBin :: (Num a, Integral a) =>  Int -> Expr a -> Parser (BinExpr a)
pBin prec l = do
    op <- try $ pBinOpWithPrecedence prec
    P.space
    r <- try (pExpr (binOpPrecedence op + 1)) <?> "right operand"
    let e = Bin op l r
    
    let lAssoc = do
        P.space
        pBin prec (BinExpr e)

    (try lAssoc <?> "left-associative binary expression") <|> return e
        

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


pRepeat :: (Num a, Integral a) =>  Int -> Expr a -> Parser (RepeatExpr a)
pRepeat prec l = do
    r <- pExpr prec
    return $ Repeat l r

