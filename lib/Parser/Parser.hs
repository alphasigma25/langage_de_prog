{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser (ParseError, parseRepl) where

import Data.Bifunctor (Bifunctor (second))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Map (Map, empty, insert, size, (!?))
import Expr (Expr (..), FctDef, Operation (..))
import Helper (readMaybeInt)
import RevString (RevString, addRS)

data ParseError
  = IncompleteExpression
  | Overflow String
  | UnrecognisedToken String
  | UnrecognizedChar Char
  | IntParseError String
  | IntOverflowError String
  | WrongToken String String
  | NotAnError

instance Show ParseError where
  show :: ParseError -> String
  show IncompleteExpression = "Incomplete expression"
  show (Overflow str) = "Overflow : " ++ str
  show (UnrecognisedToken tok) = "Unrecognised token : " ++ tok
  show (UnrecognizedChar c) = "Unrecognised char : " ++ [c]
  show (IntParseError str) = "Error while parsing int : " ++ str
  show (IntOverflowError str) = "Int overflow while parsing int : " ++ str
  show (WrongToken word test) = "Wrong token. Recieved : " ++ word ++ " Expected : " ++ test
  show NotAnError = error "Or maybe not"

type PartialParse x = (String, x) -- ce qui reste à parser + valeur

type ParsingInfos x = Either ParseError (PartialParse x)

type FctName = String

type FctCtx = Map FctName Int

type ParCtx = Map String Int

type Context = (FctCtx, ParCtx)

parseRepl :: Map String Int -> String -> Either ParseError (Either Expr (String, FctDef))
parseRepl ctx str =
  let a = Left <$> parserReplExpr ctx str -- TODO
   in either (const a) (Right . Right) $ parserReplFct ctx str

-- Cas 1
-- fact x = if x then x * fact x - 1 else 1
-- fact 5 -> 120

-- Cas 2
-- a x y z = if x then y else z
-- a 2 3 5 -> 3
-- a 0 3 5 -> 5

-- Cas 3
-- e if = 5 -> erreur parse

-- Cas 4
-- if x = x -> erreur parse

-- Cas 5
-- a x x = x -> erreur parse

-- Cassis
-- a x y = x + y
-- b x = a x 2
-- b 3 -> 5
-- a x y = x * y
-- b 3 -> 6

-- Cassette
-- a x y = x + y
-- a x = x -> erreur parse

-- Cas 8
-- a x = 0
-- b x = a x
-- a x = b x
-- a 1 -> boucle infinie

parserReplFct :: FctCtx -> String -> Either ParseError (String, FctDef)
parserReplFct context l@(c : cs)
  | isSpace c = parserReplFct context cs
  | isAlpha c =
    let (suite, name) = readText l
     in parseFct name (context, empty) suite
  | otherwise = Left $ UnrecognizedChar c
  where
    parseFct :: FctName -> Context -> String -> Either ParseError (String, FctDef)
    parseFct fctName ctx@(func, params) l2@(x : xs)
      | isSpace x = parseFct fctName ctx xs
      | x == '=' = fmap (\(_, expr) -> (fctName, (size params, expr))) (parseExpr (insert fctName (size params) func, params) xs)
      | isAlpha x =
        let (suite, newParam) = readText l2
         in let newContext = (\pctx -> insert newParam (size pctx) pctx) <$> ctx
             in parseFct fctName newContext suite
      | otherwise = Left $ UnrecognizedChar x
    parseFct _ _ [] = Left IncompleteExpression
parserReplFct _ [] = Left IncompleteExpression

parserReplExpr :: FctCtx -> String -> Either ParseError Expr
parserReplExpr context list = parseExpr (context, empty) list >>= (\(reste, parsed) -> if reste == "" then Right parsed else Left $ Overflow reste)

parserFile :: String -> a -- TODO : Futur
parserFile = undefined

parseExpr :: Context -> String -> ParsingInfos Expr
parseExpr ctx list = parseRootExpr list >>= parseInfix
  where
    parseInfix :: PartialParse Expr -> ParsingInfos Expr
    parseInfix (x : xs, expr)
      | x == '+' = fmap (fmap (Operation Addition expr)) (parseExpr ctx xs)
      | x == '*' = fmap (fmap (Operation Multiplication expr)) (parseExpr ctx xs)
      | x == '-' = fmap (fmap (Operation Soustration expr)) (parseExpr ctx xs)
      | x == '/' = fmap (fmap (Operation Division expr)) (parseExpr ctx xs)
      | isSpace x = parseInfix (xs, expr)
    parseInfix source = Right source

    parseRootExpr :: String -> ParsingInfos Expr
    parseRootExpr [] = Left IncompleteExpression
    parseRootExpr l@(x : xs)
      | isSpace x = parseRootExpr xs
      | x == '-' = fmap (fmap $ Valeur . (0 -)) (parseDigit xs)
      | isAlpha x = parseText $ readText l
      | isDigit x = fmap (fmap Valeur) (parseDigit l)
      | otherwise = Left $ UnrecognizedChar x

    parseText :: PartialParse String -> ParsingInfos Expr
    parseText (suite, mot)
      | mot == "if" = do
        (suite1, ifexpr) <- parseExpr ctx suite
        (suite11, _) <- validate suite1 "then"
        (suite2, thenexpr) <- parseExpr ctx suite11
        (suite21, _) <- validate suite2 "else"
        (suite3, elseexpr) <- parseExpr ctx suite21
        pure (suite3, If ifexpr thenexpr elseexpr)
      | otherwise =
        let callExpr = do
              nbParams <- maybe (Left $ UnrecognisedToken mot) Right $ fst ctx !? mot
              let params = parserNParam nbParams suite []
              fmap (second (Call mot)) params
         in maybe callExpr (\x -> Right (suite, ParamDef x)) (snd ctx !? mot)
      where
        parserNParam :: Int -> String -> [Expr] -> ParsingInfos [Expr]
        parserNParam 0 toParse params = Right (toParse, params)
        parserNParam nb toParse params =
          parseExpr ctx toParse >>= (\(str, expr) -> parserNParam (nb - 1) str (expr : params))

readText :: String -> PartialParse String
readText = readTextInternal mempty
  where
    readTextInternal :: RevString -> String -> PartialParse String
    readTextInternal txt (x : xs)
      | isAlphaNum x = readTextInternal (addRS x txt) xs
    readTextInternal txt list = (list, show txt)

validate :: String -> String -> ParsingInfos ()
validate toparse test =
  let (suite, mot) = readText toparse
   in if mot == test then Right (suite, ()) else Left (WrongToken mot test)

parseDigit :: String -> ParsingInfos Int
parseDigit = parseDigitInternal mempty
  where
    parseDigitInternal :: RevString -> String -> ParsingInfos Int
    parseDigitInternal digits (x : xs)
      | isDigit x = parseDigitInternal (addRS x digits) xs
    parseDigitInternal digits list =
      let revDi = show digits
       in (list,) <$> either (\x -> Left $ (if x then IntOverflowError else IntParseError) revDi) Right (readMaybeInt revDi)