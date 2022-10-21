{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser.Parser where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Grammaire.Expr
import Parser.Helper (readMaybeInt)
import Parser.RevString (RevString, add)
import Parser.Zip

data ParseError
  = IncompleteExpression String
  | UnrecognizedChar String Char String
  | IntParseError String Parsed
  | IntOverflowError String Parsed
  | WordParseError String String String
  | IncorrectWordError String String String String
  | OverflowExpression Parsed

instance Show ParseError where
  show :: ParseError -> String
  show (IncompleteExpression p) = "Incomplete expression : " ++ p
  show (OverflowExpression p) = "Overflow expression : " ++ show p
  show (UnrecognizedChar a b c) = "Invalid Char : " ++ a ++ '*' : b : '*' : c
  show (IntParseError old p) = "IntParseError : " ++ old ++ '*' : show p -- Normalement ce cas ne devrait jamais arriver
  show (IntOverflowError old p) = "IntOverflowError : " ++ old ++ '*' : show p
  show (WordParseError old word suite) = "Unrecognized word : " ++ old ++ '*' : word ++ '*' : suite
  show (IncorrectWordError expected received past future) =
    "Unexpected word : " ++ received ++ " instead of " ++ expected ++ " in " ++ past ++ '*' : received ++ '*' : future

type PartialParse x = (Parsed, x)

type ParsingInfos x = Either ParseError (PartialParse x)

parser :: String -> Either ParseError Expr
parser list = parseExpr (createParse list) >>= (\(text, result) -> if incomplete text then Left $ OverflowExpression text else Right result)

parseExpr :: Parsed -> ParsingInfos Expr
parseExpr list = parseRootExpr list >>= parseInfix

parseRootExpr :: Parsed -> ParsingInfos Expr
parseRootExpr list@(CharParsed p@(old, x, xs))
  | x == ' ' = parseRootExpr $ next p
  | x == '-' = fmap (fmap $ Valeur . (0 -)) (parseDigit $ next p)
  | isAlpha x = parseText old $ readText list
  | isDigit x = fmap (fmap Valeur) (parseDigit list)
  | otherwise = Left $ UnrecognizedChar (show old) x xs
parseRootExpr (EndParsed txt) = Left $ IncompleteExpression $ show txt

parseInfix :: PartialParse Expr -> ParsingInfos Expr
parseInfix (CharParsed p@(_, x, _), expr)
  | x == '+' = fmap (fmap (Addition expr)) (parseExpr $ next p)
  | x == '*' = fmap (fmap (Multiplication expr)) (parseExpr $ next p)
  | x == ' ' = parseInfix (next p, expr)
parseInfix source = Right source

readText :: Parsed -> PartialParse String
readText = readTextInternal mempty

readTextInternal :: RevString -> Parsed -> PartialParse String
readTextInternal txt (CharParsed p@(_, x, _))
  | isAlphaNum x = readTextInternal (add x txt) $ next p
readTextInternal txt list = (list, show txt)

parseText :: RevString -> PartialParse String -> ParsingInfos Expr
parseText old (suite, mot)
  | mot == "if" = parseIf suite
  | otherwise = Left $ WordParseError (show old) mot $ futurePart suite

parseIf :: Parsed -> ParsingInfos Expr
parseIf suite = do
  (suite1, ifexpr) <- parseExpr suite
  (suite11, _) <- validate suite1 "then"
  (suite2, thenexpr) <- parseExpr suite11
  (suite21, _) <- validate suite2 "else"
  (suite3, elseexpr) <- parseExpr suite21
  pure (suite3, If ifexpr thenexpr elseexpr)

validate :: Parsed -> String -> ParsingInfos ()
validate toparse test =
  let (suite, mot) = readText toparse
   in if mot == test then Right (suite, ()) else Left $ IncorrectWordError test mot (show $ parsedPart toparse) $ futurePart suite

parseDigit :: Parsed -> ParsingInfos Int
parseDigit suite =
  let old = parsedPart suite
   in let remergeContext (a, b) = (merge a old, b)
       in remergeContext <$> parseDigitInternal old mempty (createParse $ futurePart suite)

parseDigitInternal :: RevString -> RevString -> Parsed -> ParsingInfos Int
parseDigitInternal old digits (CharParsed p@(_, x, _))
  | isDigit x = parseDigitInternal old (add x digits) $ next p
parseDigitInternal old digits list =
  (list,) <$> either (\x -> Left $ (if x then IntParseError else IntOverflowError) (show old) list) Right (readMaybeInt $ show digits)