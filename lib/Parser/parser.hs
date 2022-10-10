{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TupleSections #-}

module Parser.Parser where

import Data.Char (isDigit)
import Grammaire.Expr (Expr (Addition, Multiplication, Valeur))
import Text.Read (readMaybe)

data ParseError 
  = IncompleteExpression 
  | UnrecognizedChar Char 
  | IntParseError String 
  deriving Show

type PartialParse x = (String, x)

type ParsingInfos x = Either ParseError (PartialParse x)

parser :: String -> Either ParseError Expr
parser list = parseExpr list >>= (\x -> if fst x == "" then Right (snd x) else Left IncompleteExpression)

parseExpr :: String -> ParsingInfos Expr
parseExpr list = parseInfix <$> parseRootExpr list

parseRootExpr :: String -> ParsingInfos Expr
parseRootExpr list@(x : xs)
  | x == ' ' = parseRootExpr xs
  | x == '-' = fmap (fmap $ Valeur . (0-)) (parseDigit "" xs)
  | x == 'i' = undefined -- 4
  | isDigit x = fmap (fmap Valeur) (parseDigit "" list)
  | otherwise = Left $ UnrecognizedChar x
parseRootExpr [] = Left IncompleteExpression

parseInfix :: PartialParse Expr -> PartialParse Expr
parseInfix source@(x : xs, expr)
  | x == '+' = either (const source) (fmap (Addition expr)) (parseExpr xs)
  | x == '*' = either (const source) (fmap (Multiplication expr)) (parseExpr xs)
  | x == ' ' = parseInfix (xs, expr)
parseInfix source = source

parseDigit :: String -> String -> ParsingInfos Int
parseDigit (x : xs) digits
  | isDigit x = parseDigit xs (x : digits)
parseDigit list digits =
  let revDigits = reverse digits in
    (list, ) <$> maybe (Left $ IntParseError revDigits) Right (readMaybeInt $ reverse revDigits)

readMaybeInt :: String -> Maybe Int
readMaybeInt a = readMaybe a >>= applyFilter

applyFilter :: Integer -> Maybe Int
applyFilter a = if filterIntInteger a then Just $ fromInteger a else Nothing

filterIntInteger :: Integer -> Bool
filterIntInteger a = (a < toInteger (maxBound :: Int)) && (a > toInteger (minBound :: Int))

maybeToEither :: Maybe a -> b -> Either b a
maybeToEither a b = maybe (Left b) Right a