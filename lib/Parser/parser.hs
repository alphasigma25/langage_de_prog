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
  | WrongToken String
  deriving (Show)

type PartialParse x = (String, x) -- ce qui reste à parser + valeur

type ParsingInfos x = Either ParseError (PartialParse x)

parser :: String -> Either ParseError Expr
parser list = parseExpr list >>= (\x -> if fst x == "" then Right (snd x) else Left IncompleteExpression)

parseExpr :: String -> ParsingInfos Expr
parseExpr list = parseInfix <$> parseRootExpr list
  where
    parseInfix :: PartialParse Expr -> PartialParse Expr
    parseInfix source@(x : xs, expr)
      | x == '+' = either (const source) (fmap (Addition expr)) (parseExpr xs)
      | x == '*' = either (const source) (fmap (Multiplication expr)) (parseExpr xs)
      | x == ' ' = parseInfix (xs, expr)
    parseInfix source = source

    parseRootExpr :: String -> ParsingInfos Expr
    parseRootExpr [] = Left IncompleteExpression
    parseRootExpr l@(x : xs)
      | x == ' ' = parseRootExpr xs
      | x == '-' = fmap (fmap $ Valeur . (0 -)) (parseDigit xs)
      | isAlpha x = parseText $ readText l
      | isDigit x = fmap (fmap Valeur) (parseDigit l)
      | otherwise = Left $ UnrecognizedChar x
      where
        parseText :: PartialParse String -> ParsingInfos Expr
        parseText (suite, mot)
          | mot == "if" = parseIf suite
          | otherwise = Left IncompleteExpression
          where
            parseIf :: String -> ParsingInfos Expr
            parseIf suite0 = do
              (suite1, ifexpr) <- parseExpr suite0
              (suite11, _) <- validate suite1 "then"
              (suite2, thenexpr) <- parseExpr suite11
              (suite21, _) <- validate suite2 "else"
              (suite3, elseexpr) <- parseExpr suite21
              pure (suite3, If ifexpr thenexpr elseexpr)

readText :: String -> PartialParse String
readText = readTextInternal ""
  where
    readTextInternal :: String -> String -> PartialParse String
    readTextInternal txt (x : xs)
      | isAlphaNum x = readTextInternal (x : txt) xs
    readTextInternal txt list = (list, reverse txt)

validate :: String -> String -> ParsingInfos ()
validate toparse test =
  let (suite, mot) = readText toparse
   in if mot == test then Right (suite, ()) else Left (WrongToken mot)

parseDigit :: String -> ParsingInfos Int
parseDigit = parseDigitInternal ""
  where
    parseDigitInternal :: String -> String -> ParsingInfos Int
    parseDigitInternal digits (x : xs)
      | isDigit x = parseDigitInternal (x : digits) xs
    parseDigitInternal digits list =
      let revDi = reverse digits
       in (list,) <$> maybe (Left $ IntParseError revDi) Right (readMaybeInt revDi)