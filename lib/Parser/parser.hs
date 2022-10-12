{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser.Parser where

import Data.Char (isDigit)
import Grammaire.Expr
import Parser.Helper

data ParseError
  = IncompleteExpression
  | UnrecognizedChar Char
  | IntParseError String
  deriving (Show)

type PartialParse x = (String, x)

type ParsingInfos x = Either ParseError (PartialParse x)

parser :: String -> Either ParseError Expr
parser list = parseExpr list >>= (\x -> if fst x == "" then Right (snd x) else Left IncompleteExpression)

parseExpr :: String -> ParsingInfos Expr
parseExpr list = parseInfix <$> parseRootExpr list

parseRootExpr :: String -> ParsingInfos Expr
parseRootExpr list@(x : xs)
  | x == ' ' = parseRootExpr xs
  | x == '-' = fmap (fmap $ Valeur . (0 -)) (parseDigit xs)
  | x == 'i' = undefined -- 4
  | isDigit x = fmap (fmap Valeur) (parseDigit list)
  | otherwise = Left $ UnrecognizedChar x
parseRootExpr [] = Left IncompleteExpression

parseInfix :: PartialParse Expr -> PartialParse Expr
parseInfix source@(x : xs, expr)
  | x == '+' = either (const source) (fmap (Addition expr)) (parseExpr xs)
  | x == '*' = either (const source) (fmap (Multiplication expr)) (parseExpr xs)
  | x == ' ' = parseInfix (xs, expr)
parseInfix source = source

parseDigit :: String -> ParsingInfos Int
parseDigit = parseDigitInternal ""

parseDigitInternal :: String -> String -> ParsingInfos Int
parseDigitInternal digits (x : xs)
  | isDigit x = parseDigitInternal (x : digits) xs
parseDigitInternal digits list =
  (list,) <$> maybe (Left $ IntParseError digits) Right (readMaybeInt $ reverse digits)