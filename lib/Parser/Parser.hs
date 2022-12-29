{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser (ParseError, parseRepl, parserReplExpr, parserReplFct, parserFile, TestFctExpr (..)) where

import Data.Bifunctor (Bifunctor (second))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Foldable (foldlM)
import Data.Int (Int16)
import Data.Map (Map, empty, insert, member, size, (!?))
import Expr (Expr (..), FctDef, Op (..), int16ToInt, size16)
import Helper (readMaybeInt)
import RevString (RevString, addRS, isWhiteSpace)

data ParseError
  = IncompleteExpression
  | Overflow String
  | UnrecognisedToken String
  | UnrecognizedChar Char
  | IntParseError String
  | IntOverflowError String
  | WrongToken String String
  | ReservedToken String
  | DuplicatedParamName String
  | MultipleFctDef String
  | InvalidParamCount Int Int16
  | NotFuncError Char
  | NotFunc
  | NotAnError

instance Show ParseError where
  show :: ParseError -> String
  show IncompleteExpression = "Incomplete expression"
  show NotFunc = "Incomplete expression"
  show (Overflow str) = "Overflow : " ++ str
  show (UnrecognisedToken tok) = "Unrecognised token : " ++ tok
  show (UnrecognizedChar c) = "Unrecognised char : " ++ [c]
  show (NotFuncError c) = "Unrecognised char : " ++ [c]
  show (IntParseError str) = "Error while parsing int : " ++ str
  show (IntOverflowError str) = "Int overflow while parsing int : " ++ str
  show (WrongToken word test) = "Wrong token. Recieved : " ++ word ++ " Expected : " ++ test
  show (ReservedToken str) = "Reserved token used in param of function definition : " ++ str
  show (DuplicatedParamName str) = "Duplicated parameter name : " ++ str
  show (MultipleFctDef str) = "Multiple function definition with name : " ++ str
  show (InvalidParamCount e r) = "Wrong token. Expected : " ++ show e ++ " Recieved : " ++ show r
  show NotAnError = error "Or maybe not"

type PartialParse x = (String, x) -- ce qui reste à parser + valeur

type ParsingInfos x = Either ParseError (PartialParse x)

type FctName = String

type FctCtx = Map FctName Int16 -- nom fct, nb params

type ParCtx = Map String Int16 -- nom param, index param

type Context = (FctCtx, ParCtx)

data TestFctExpr = Err ParseError | Expr Expr | Fct (FctName, FctDef)

parseRepl :: Map String Int16 -> String -> TestFctExpr
parseRepl ctx str = either testFunc Fct $ parserReplFct ctx str
  where
    testFunc :: ParseError -> TestFctExpr
    testFunc (NotFuncError _) = parseExprInstead
    testFunc (ReservedToken _) = parseExprInstead
    testFunc NotFunc = parseExprInstead
    testFunc err = Err err

    parseExprInstead :: TestFctExpr
    parseExprInstead = either Err Expr $ parserReplExpr ctx str

testFctName :: String -> Either ParseError ()
testFctName "if" = Left (ReservedToken "if")
testFctName "then" = Left (ReservedToken "then")
testFctName "else" = Left (ReservedToken "else")
testFctName "let" = Left (ReservedToken "let")
testFctName "in" = Left (ReservedToken "in")
testFctName _ = pure ()

parserReplFct :: FctCtx -> String -> Either ParseError (String, FctDef)
parserReplFct context l = do
  (name, suite) <- parseFctName l
  parseCtx empty suite >>= parseFct name context
  where
    parseFct :: FctName -> FctCtx -> (ParCtx, String) -> Either ParseError (String, FctDef)
    parseFct fctName func (params, suite) =
      let redefError = (func !? fctName) >>= (\nbParams -> if int16ToInt nbParams == size params then Nothing else Just (InvalidParamCount (size params) nbParams))
       in let funcDef = parseTerminalExpr (insert fctName (size16 params) func, params) suite
           in maybe (fmap (\expr -> (fctName, (size16 params, expr))) funcDef) Left redefError

parseCtx :: ParCtx -> String -> Either ParseError (ParCtx, String)
parseCtx params l2@(x : xs)
  | isSpace x = parseCtx params xs
  | x == '=' = Right (params, xs)
  | isAlpha x =
    let (suite, newParam) = readText l2
     in let newContext = insert newParam (size16 params) params
         in testFctName newParam >> if member newParam params then Left (DuplicatedParamName newParam) else parseCtx newContext suite
  | otherwise = Left $ NotFuncError x
parseCtx _ [] = Left NotFunc

parserReplExpr :: FctCtx -> String -> Either ParseError Expr
parserReplExpr context = parseTerminalExpr (context, empty)

mergeTuple :: a -> (b, c) -> (a, b, c)
mergeTuple a (b, c) = (a, b, c)

parserFile :: String -> Either ParseError [(FctName, FctDef)]
parserFile str = do
  let a = filter (not . isWhiteSpace) (foldl accumulate [] str)
  b <- traverse (parseFctName . show) a
  c <- traverse (\(name, suite) -> mergeTuple name <$> parseCtx empty suite) b
  fctctx <- foldlM (\m (name, pctx, _) -> insertInMap name (size16 pctx) m) empty c
  traverse (\(name, pctx, suite) -> (name,) . (size16 pctx,) <$> parseTerminalExpr (fctctx, pctx) suite) c
  where
    accumulate :: [RevString] -> Char -> [RevString]
    accumulate rstr '.' = mempty : rstr
    accumulate (x : xs) c = addRS c x : xs
    accumulate [] c = [addRS c mempty]

    insertInMap :: String -> a -> Map String a -> Either ParseError (Map String a)
    insertInMap name v m = if member name m then Left $ MultipleFctDef name else Right $ insert name v m

parseFctName :: String -> Either ParseError (FctName, String)
parseFctName l@(c : cs)
  | isSpace c = parseFctName cs
  | isAlpha c =
    let (suite, name) = readText l
     in (name, suite) <$ testFctName name
  | otherwise = Left $ NotFuncError c
parseFctName [] = Left NotFunc

parseTerminalExpr :: Context -> String -> Either ParseError Expr
parseTerminalExpr context list = parseExpr context list >>= (\(reste, parsed) -> if reste == "" then Right parsed else Left $ Overflow reste)

parseExpr :: Context -> String -> ParsingInfos Expr
parseExpr ctx list = parseRootExpr list >>= parseInfix
  where
    parseInfix :: PartialParse Expr -> ParsingInfos Expr
    parseInfix (x : xs, expr)
      | x == '+' = fmap (fmap (Operation Add expr)) (parseExpr ctx xs)
      | x == '*' = fmap (fmap (Operation Mul expr)) (parseExpr ctx xs)
      | x == '-' = fmap (fmap (Operation Sub expr)) (parseExpr ctx xs)
      | x == '/' = fmap (fmap (Operation Div expr)) (parseExpr ctx xs)
      | x == '%' = fmap (fmap (Operation Mod expr)) (parseExpr ctx xs)
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
              let params = (reverse <$>) <$> parserNParam (int16ToInt nbParams) suite []
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

parseDigit :: String -> ParsingInfos Int16
parseDigit = parseDigitInternal mempty
  where
    parseDigitInternal :: RevString -> String -> ParsingInfos Int16
    parseDigitInternal digits (x : xs)
      | isDigit x = parseDigitInternal (addRS x digits) xs
    parseDigitInternal digits list =
      let revDi = show digits
       in (list,) <$> either (\x -> Left $ (if x then IntOverflowError else IntParseError) revDi) Right (readMaybeInt revDi)