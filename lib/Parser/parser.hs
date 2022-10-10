{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}

module Parser.Parser where

import Data.Char
import Grammaire.Expr

data ParseError = IncompleteExpression | UnrecognizedChar Char

type PartialParse x = (x, String)

parser :: String -> Either ParseError Expr
parser list = parseExpr list >>= (\x -> if snd x == "" then Right (fst x) else Left IncompleteExpression)

parseExpr :: String -> Either ParseError (PartialParse Expr)
parseExpr list = Right $ parseInfix (parseRootExpr list)
  -- | canParseRoot x
  -- | otherwise = Left $ UnrecognizedChar x

parseRootExpr :: String -> Either ParseError (PartialParse Expr)
parseRootExpr list@(x:xs)
  | x == ' ' = parseRootExpr xs
  | x == '-' = undefined -- 3
  | x == 'i' = undefined  -- 4
  | isDigit x = undefined -- 2
  | otherwise = undefined -- 1
parseRootExpr [] = Left IncompleteExpression

parseInfix :: PartialParse Expr -> PartialParse Expr
parseInfix source@(expr, x : xs)
  | x == '+' = either (const source) (\val -> (Addition expr $ fst val, snd val)) (parseExpr xs)
  | x == '*' = either (const source) (\val -> (Multiplication expr $ fst val, snd val)) (parseExpr xs)
  | x == ' ' = parseInfix (expr, xs)
  | otherwise = source
parseInfix source = source