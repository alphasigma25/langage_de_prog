{-# LANGUAGE InstanceSigs #-}

module Parser.CommonParser where

import Parser.Zip (Parsed)
import qualified Text.Show

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