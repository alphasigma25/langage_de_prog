{-# LANGUAGE InstanceSigs #-}

module ParsingError where

import qualified Text.Show
import Zip (Parsed)

data ParseError
  = IncompleteExpression Text
  | UnrecognizedChar Text Char Text
  | IntParseError Text Parsed
  | IntOverflowError Text Parsed
  | WordParseError Text Text Text
  | IncorrectWordError Text Text Text Text
  | OverflowExpression Parsed
  | EmptyProgramException Text
  | NotAFunc Text
  | UnrecognizedFuncChar Text Char Text
  | InvalidFuncNameError
  | InvalidParamNameError
  | InvalidParamCountNumber

instance Show ParseError where
  show :: ParseError -> String
  show (IncompleteExpression p) = "Incomplete expression : " ++ toString p
  show (NotAFunc p) = "Incomplete expression : " ++ toString p
  show (OverflowExpression p) = "Overflow expression : " ++ show p
  show (UnrecognizedChar a b c) = "Invalid Char : " ++ toString a ++ '*' : b : '*' : toString c
  show (UnrecognizedFuncChar a b c) = "Invalid Char : " ++ toString a ++ '*' : b : '*' : toString c
  show (IntParseError old p) = "IntParseError : " ++ toString old ++ '*' : show p -- Normalement ce cas ne devrait jamais arriver
  show (IntOverflowError old p) = "IntOverflowError : " ++ toString old ++ '*' : show p
  show (WordParseError old word suite) = "Unrecognized word : " ++ toString old ++ '*' : toString word ++ '*' : toString suite
  show (EmptyProgramException txt) = "EmptyProgram : " ++ toString txt
  show InvalidFuncNameError = "The name if is not a valid name for a function"
  show InvalidParamNameError = "The name if is not a valid name for a param"
  show InvalidParamCountNumber = "Invalid param count number"
  show (IncorrectWordError expected received past future) =
    "Unexpected word : " ++ toString received ++ " instead of " ++ toString expected ++ " in " ++ toString past ++ '*' : toString received ++ '*' : toString future

notAFuncError :: ParseError -> Bool
notAFuncError (NotAFunc _) = True
notAFuncError UnrecognizedFuncChar {} = True
notAFuncError InvalidParamNameError = True
notAFuncError InvalidFuncNameError = True
notAFuncError _ = False