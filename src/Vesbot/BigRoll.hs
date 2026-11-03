{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
{- HLint ignore "Use newtype instead of data" -}

module Vesbot.BigRoll (
    Expr, RolledExpr, roll, eval, parse
) where

import Control.Applicative ((<|>))

import qualified Debug.Trace (trace)
import Data.Attoparsec.Combinator ((<?>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.List as List (intercalate)

import System.Random (SplitGen, uniformR)
import Control.Monad (when)
import Data.Maybe (listToMaybe)

data Expr
    = UnaryExpr  UnaryExpr
    | BinExpr    BinExpr
    | DivExpr    DivExpr
    | RepeatExpr RepeatExpr
    | DieExpr    DieExpr
    | ConstExpr  ConstExpr
 deriving (Show, Eq)

data UnaryExpr  = Unary  UnaryOp Expr      deriving (Show, Eq)
data BinExpr    = Bin    BinOp   Expr Expr deriving (Show, Eq)
data DivExpr    = Div    Expr    ConstExpr deriving (Show, Eq)
data RepeatExpr = Repeat         Expr Expr deriving (Show, Eq)
data DieExpr    = Die    Int               deriving (Show, Eq)
data ConstExpr  = Const  Int               deriving (Show, Eq)

data UnaryOp = Neg           deriving (Show, Eq)
data BinOp = Add | Sub | Mul deriving (Show, Eq)

binOpPrecedence :: BinOp -> Int
binOpPrecedence Add = 5
binOpPrecedence Sub = 5
binOpPrecedence Mul = 8
divOpPrecedence :: Int
divOpPrecedence = 8

data RolledExpr
    = RolledUnaryExpr  RolledUnaryExpr
    | RolledBinExpr    RolledBinExpr
    | RolledDivExpr    RolledDivExpr
    | RolledRepeatExpr RolledRepeatExpr
    | RolledDieExpr    RolledDieExpr
    | RolledConstExpr  RolledConstExpr
 deriving (Show, Eq)

data RolledUnaryExpr  = RolledUnary  UnaryOp RolledExpr                 deriving (Show, Eq)
data RolledBinExpr    = RolledBin    BinOp   RolledExpr RolledExpr      deriving (Show, Eq)
data RolledDivExpr    = RolledDiv            RolledExpr RolledConstExpr deriving (Show, Eq)
data RolledRepeatExpr = RolledRepeat [RolledExpr]                       deriving (Show, Eq)
data RolledDieExpr    = RolledDie    Int                                deriving (Show, Eq)
data RolledConstExpr  = RolledConst  Int                                deriving (Show, Eq)

roll :: SplitGen g => Expr -> g -> (RolledExpr, g)
roll (UnaryExpr  e) = mapFst RolledUnaryExpr  . rollUnary  e
roll (BinExpr    e) = mapFst RolledBinExpr    . rollBin    e
roll (DivExpr    e) = mapFst RolledDivExpr    . rollDiv    e
roll (RepeatExpr e) = mapFst RolledRepeatExpr . rollRepeat e
roll (DieExpr    e) = mapFst RolledDieExpr    . rollDie    e
roll (ConstExpr  e) = mapFst RolledConstExpr  . rollConst  e

rollUnary :: SplitGen g => UnaryExpr -> g -> (RolledUnaryExpr, g)
rollUnary (Unary op e) rng = let
    (e', rng') = roll e rng
 in (RolledUnary op e', rng')
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
rollDie :: SplitGen g => DieExpr -> g -> (RolledDieExpr, g)
rollDie (Die n) rng = let
    range      = if n < 0 then (n, 0) else (0, n)
    (n', rng') = uniformR range rng
 in (RolledDie n', rng')
rollConst :: SplitGen g => ConstExpr -> g -> (RolledConstExpr, g)
rollConst (Const n) = (RolledConst n,)


eval :: RolledExpr -> Int
eval (RolledUnaryExpr  e) = evalUnary  e
eval (RolledBinExpr    e) = evalBin    e
eval (RolledDivExpr    e) = evalDiv    e
eval (RolledRepeatExpr e) = evalRepeat e
eval (RolledDieExpr    e) = evalDie    e
eval (RolledConstExpr  e) = evalConst  e

evalUnary :: RolledUnaryExpr -> Int
evalUnary (RolledUnary Neg e) = -(eval e)
evalBin :: RolledBinExpr -> Int
evalBin (RolledBin Add l r) = eval l + eval r
evalBin (RolledBin Sub l r) = eval l - eval r
evalBin (RolledBin Mul l r) = eval l * eval r
evalDiv :: RolledDivExpr -> Int
evalDiv (RolledDiv l r) = eval l `div` evalConst r
evalRepeat :: RolledRepeatExpr -> Int
evalRepeat (RolledRepeat es) = sum . map eval $ es
evalDie :: RolledDieExpr -> Int
evalDie (RolledDie n) = n
evalConst :: RolledConstExpr -> Int
evalConst (RolledConst n) = n


parse :: Trace -> Atto.Parser Expr
parse stack =
    let stack' = pushFrame "parse" [] stack in
    trace stack' $
    parseExpr 0 stack' <* Atto.endOfInput 

-- TODO: figure out proper backtracking

parseExpr :: Int -> Trace -> Atto.Parser Expr
parseExpr minPrecedence stack = let stack' = pushFrame "expr" [show minPrecedence] stack in
    trace stack' $
    label "expression" $
    parseGrouped    (parseExpr 0)                  stack' <|>
    UnaryExpr    <$> parseUnaryExpr                stack' <|>
    BinExpr      <$> parseBinExpr    minPrecedence stack' <|>
    DivExpr      <$> parseDivExpr    minPrecedence stack' <|>
    RepeatExpr   <$> parseRepeatExpr minPrecedence stack' <|>
    DieExpr      <$> parseDieExpr                  stack' <|>
    ConstExpr    <$> parseConstExpr                stack'
    

-- Always consumes input or fails
parseProgressiveExpr :: Trace -> Atto.Parser Expr
parseProgressiveExpr stack =
    let stack' = pushFrame "progressiveExpr" [] stack in
    trace stack' $
    parseGrouped    (parseExpr 0)   stack' <|>
    UnaryExpr    <$> parseUnaryExpr stack' <|>
    DieExpr      <$> parseDieExpr   stack' <|>
    ConstExpr    <$> parseConstExpr stack'

parseGrouped :: Show a => (Trace -> Atto.Parser a) -> Trace -> Atto.Parser a
parseGrouped p stack =
    let stack' = pushFrame "grouped" [] stack in
    trace stack' $
    label "grouped expression" $ do
    _ <- label ")" (Atto.string "(")
    Atto.skipSpace
    e <- p stack'
    Atto.skipSpace
    _ <- label ")" (Atto.string ")")
    return e

{-# ANN parseUnaryExpr ( "HLint: ignore Use <$>" :: String ) #-}
parseUnaryExpr :: Trace -> Atto.Parser UnaryExpr
parseUnaryExpr stack =
    let stack' = pushFrame "unaryExpr" [] stack in
    trace stack' $
    label "unary expression" $ do
    op <- parseUnaryOp stack'
    Atto.skipSpace
    e  <- parseProgressiveExpr stack'
    return $ Unary op e

parseUnaryOp :: Trace -> Atto.Parser UnaryOp
parseUnaryOp stack =
    let stack' = pushFrame "unaryOp" [] stack in
    trace stack' $
    label "unary operator" $ do
    Neg <$ Atto.string "-"

{-# ANN parseBinExpr ( "HLint: ignore Use <$>" :: String ) #-}
parseBinExpr :: Int -> Trace -> Atto.Parser BinExpr
parseBinExpr minPrecedence stack =
    let stack' = pushFrame "binExpr" [show minPrecedence] stack in
    trace stack' $
    label "binary expression" $ do
    left  <-  parseProgressiveExpr stack'
    parseBinExpr' left minPrecedence stack'


parseBinExpr' :: Expr -> Int -> Trace -> Atto.Parser BinExpr
parseBinExpr' left minPrecedence stack =
    let stack' = pushFrame "binExpr'" [show left, show minPrecedence] stack in
    trace stack' $ do
    Atto.skipSpace
    op    <- parseBinOp stack'
    checkPrecedence minPrecedence (binOpPrecedence op) 
    Atto.skipSpace
    right <- parseExpr (binOpPrecedence op + 1) stack'
    let e = Bin op left right
    parseBinExpr' (BinExpr e) minPrecedence stack' <|> return e

parseBinOp :: Trace -> Atto.Parser BinOp
parseBinOp stack =
    let stack' = pushFrame "binOp" [] stack in
    trace stack' $
    label "binary operator" $
    (Add <$ Atto.string "+") <|>
    (Sub <$ Atto.string "-") <|>
    (Mul <$ Atto.string "*")

{-# ANN parseDivExpr ( "HLint: ignore Use <$>" :: String ) #-}
parseDivExpr :: Int -> Trace -> Atto.Parser DivExpr
parseDivExpr minPrecedence stack =
    let stack' = pushFrame "divExpr" [show minPrecedence] stack in
    trace stack' $
    label "division expression" $ do
    left <- parseProgressiveExpr stack'
    Atto.skipSpace
    _ <- Atto.string "/"
    checkPrecedence minPrecedence divOpPrecedence
    Atto.skipSpace
    right <- parseNonZeroConstExpr stack'
    return $ Div left right

checkPrecedence :: Int -> Int -> Atto.Parser ()
checkPrecedence minPrecedence  precedence =
    when (precedence < minPrecedence) $ do
        _ <- traceMsg $ unwords ["failure:", "precedence", show precedence, "<", show minPrecedence]
        fail "precedence"

{-# ANN parseRepeatExpr ( "HLint: ignore Use <$>" :: String ) #-}
parseRepeatExpr :: Int -> Trace -> Atto.Parser RepeatExpr
parseRepeatExpr minPrecedence stack =
    let stack' = pushFrame "repeatExpr" [show minPrecedence] stack in
    trace stack' $ 
    label "repeat expression" $ do
    left  <- parseProgressiveExpr stack'
    Atto.skipSpace
    right <- parseExpr minPrecedence stack'
    return $ Repeat left right

{-# ANN parseDieExpr ( "HLint: ignore Use <$>" :: String ) #-}
parseDieExpr :: Trace -> Atto.Parser DieExpr
parseDieExpr stack =
    let stack' = pushFrame "dieExpr" [] stack in
    trace stack' $ 
    label "die expression" $ do
    _ <- (Atto.string "d" <?> "d") <|> (Atto.string "D" <?> "D")
    n <- parseConst stack'
    return $ Die n

{-# ANN parseConstExpr ( "HLint: ignore Use <$>" :: String ) #-}
parseConstExpr :: Trace -> Atto.Parser ConstExpr
parseConstExpr stack =
    let stack' = pushFrame "constExpr" [] stack in
    trace stack'  $
    label "constant expression" $ do
    n <- parseConst stack'
    return $ Const n

parseNonZeroConstExpr :: Trace -> Atto.Parser ConstExpr
parseNonZeroConstExpr stack =
    let stack' = pushFrame "nonZeroConstExpr" [] stack in
    trace stack' $ Const <$> parseNonZeroConst stack'

parseNonZeroConst :: Trace -> Atto.Parser Int
parseNonZeroConst stack =
    let stack' = pushFrame "nonZeroConst" [] stack in
    trace stack' $
    label "non-zero constant" $ do
    n <- parseConst stack'
    when (n == 0) (fail "constant is zero")
    return n

parseConst :: Trace -> Atto.Parser Int
parseConst stack =
    let stack' = pushFrame "const" [] stack in
    trace stack' $
    label "constant" $ do
    sign <- (-1 <$ Atto.string "-") <|> return 1
    n    <- Atto.decimal
    return $ sign * n

parseTest :: String -> Atto.Parser a -> Either String a
parseTest s p = Atto.parseOnly p $ BS.pack s

type TraceFrame = forall a. Show a => [ParseFrame] -> Atto.Parser a -> Atto.Parser a
data Trace = Trace [ParseFrame] TraceFrame
data ParseFrame = Frame String [String]

newTrace :: TraceFrame -> Trace
newTrace = Trace []
pushFrame :: String -> [String] -> Trace -> Trace
pushFrame name args (Trace stack f) = Trace (Frame name args : stack) f
frameName :: ParseFrame -> String
frameName (Frame name _) = name
frameArgs :: ParseFrame -> [String]
frameArgs (Frame _ args) = args

trace :: Show a => Trace -> Atto.Parser a -> Atto.Parser a
trace (Trace stack f) = f stack

noopTrace :: Show a => [ParseFrame] -> Atto.Parser a -> Atto.Parser a
noopTrace = const id

traceParser :: Show a => [ParseFrame] -> Atto.Parser a -> Atto.Parser a
traceParser stack p = do
    let names = (\(Frame name _) -> name) <$> reverse stack
    let args = maybe [] (\ (Frame _ a) -> a) (listToMaybe stack)
    let depth = padLeft ' ' 3 . show . length $ stack
    let stackMsg = depth <> " " <> List.intercalate "." names
    
    let nextMsg = maybe ["EOI"] pure
    pre <- nextChunk
    let preMsg = unwords (stackMsg : args ++ nextMsg pre)
    _ <- traceMsg preMsg
    
    r <- p <|> do
        _ <- traceMsg $ preMsg <> ": failure "
        fail ""
    
    post <- nextChunk
    _ <- traceMsg $ preMsg <> ": success: " <> unwords (show r : nextMsg post)
    
    return r

nextChunk :: Atto.Parser (Maybe String)
nextChunk = do
    next <- Atto.getChunk
    return $ show <$> next

traceMsg :: String -> Atto.Parser ()
traceMsg msg = do
    Debug.Trace.trace msg (return ())

label :: String -> Atto.Parser a -> Atto.Parser a
label = flip (<?>)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (l, r) = (f l, r)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (l, r) = (l, f r)

padLeft :: a -> Int -> [a] -> [a]
padLeft x n xs
    | length xs > n = xs
    | otherwise     = replicate (n - length xs) x ++ xs