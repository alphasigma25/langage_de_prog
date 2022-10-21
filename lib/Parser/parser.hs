{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser.Parser where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Grammaire.Expr
import Parser.Helper (readMaybeInt)

data ParseError
  = IncompleteExpression
  | UnrecognizedChar Char
  | IntParseError String
  | IntOverflowError String
  | WordParseError String
  | IncorrectWordError String String
  | OverflowExpression String

instance Show ParseError where
  show :: ParseError -> String
  show IncompleteExpression = "Incomplete expression"
  show (OverflowExpression suite) = "Overflow expression : *" ++ suite ++ "*"
  show (UnrecognizedChar a) = "Invalid Char : " ++ a : ""
  show (IntParseError txt) = "Int parse error : " ++ txt
  show (IntOverflowError txt) = "IntOverflowError : " ++ txt
  show (WordParseError txt) = "Unrecognized word : " ++ txt
  show (IncorrectWordError txt txt2) =
    "Unexpected word : " ++ txt ++ " instead of " ++ txt2

type PartialParse x = (String, x) -- ce qui reste à parser + valeur

type ParsingInfos x = Either ParseError (PartialParse x)

parser :: String -> Either ParseError Expr
parser list = parseExpr list >>= (\(txt, expr) -> if txt == "" then Right expr else Left $ OverflowExpression txt)

parseExpr :: String -> ParsingInfos Expr
parseExpr list = parseInfix <$> parseRootExpr list

parseRootExpr :: String -> ParsingInfos Expr
parseRootExpr list@(x : xs)
  | x == ' ' = parseRootExpr xs
  | x == '-' = fmap (fmap $ Valeur . (0 -)) (parseDigit xs)
  | isAlpha x = parseText $ readText list
  | isDigit x = fmap (fmap Valeur) (parseDigit list)
  | otherwise = Left $ UnrecognizedChar x
parseRootExpr [] = Left IncompleteExpression

parseInfix :: PartialParse Expr -> PartialParse Expr
parseInfix source@(x : xs, expr)
  | x == '+' = either (const source) (fmap (Addition expr)) (parseExpr xs)
  | x == '*' = either (const source) (fmap (Multiplication expr)) (parseExpr xs)
  | x == ' ' = parseInfix (xs, expr)
parseInfix source = source

readText :: String -> PartialParse String
readText = readTextInternal ""

readTextInternal :: String -> String -> PartialParse String
readTextInternal txt (x : xs)
  | isAlphaNum x = readTextInternal (x : txt) xs
readTextInternal txt list = (list, reverse txt)

parseText :: PartialParse String -> ParsingInfos Expr
parseText (suite, mot)
  | mot == "if" = parseIf suite
  | otherwise = Left $ WordParseError mot

parseIf :: String -> ParsingInfos Expr
parseIf suite = do
  (suite1, ifexpr) <- parseExpr suite
  (suite11, _) <- validate suite1 "then"
  (suite2, thenexpr) <- parseExpr suite11
  (suite21, _) <- validate suite2 "else"
  (suite3, elseexpr) <- parseExpr suite21
  pure (suite3, If ifexpr thenexpr elseexpr)

validate :: String -> String -> ParsingInfos ()
validate toparse test =
  let (suite, mot) = readText toparse
   in if mot == test then Right (suite, ()) else Left (IncorrectWordError mot test)

parseDigit :: String -> ParsingInfos Int
parseDigit = parseDigitInternal ""

parseDigitInternal :: String -> String -> ParsingInfos Int
parseDigitInternal digits (x : xs)
  | isDigit x = parseDigitInternal (x : digits) xs
parseDigitInternal digits list =
  let revDi = reverse digits
   in (list,) <$> either (\x -> Left $ (if x then IntParseError else IntOverflowError) revDi) Right (readMaybeInt revDi)