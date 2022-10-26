{-# LANGUAGE TupleSections #-}

module ExpressionParser where

import CommonParser (MaybeParse, PartialParse, isWhitespace, readText)
import Context (FonctionParsingContext, ParamParsingContext)
import Data.Char (isAlpha, isDigit)
import Data.Map ((!?))
import Expr
import Helper (readMaybeInt)
import ParsingError (ParseError (..))
import RevString (RevList, RevString, add, toListRev, toTextRev)
import Zip (Parsed (..), createParse, futurePart, merge, next, parsedPart)

newtype ParsingContext = ParsingContext (FonctionParsingContext, ParamParsingContext)

tryExtractParam :: ParsingContext -> Text -> Maybe Int
tryExtractParam (ParsingContext (_, params)) name = params !? name

tryExtractFunction :: ParsingContext -> Text -> Maybe Int
tryExtractFunction (ParsingContext (mutFuncs, _)) name = mutFuncs !? FonctionName name

parseExprInternal :: ParsingContext -> Parsed -> MaybeParse Expr
parseExprInternal context texte = parseRootExpr texte >>= parseInfix
  where
    parseInfix :: PartialParse Expr -> MaybeParse Expr
    parseInfix (CharParsed p@(_, x, _), expr)
      | x == '+' = parseOp OpAdd
      | x == '*' = parseOp OpMult
      | x == '-' = parseOp OpSous
      | x == '/' = parseOp OpDiv
      | isWhitespace x = parseInfix (next p, expr)
      where
        parseOp :: Operation -> MaybeParse Expr
        parseOp opType = Operation opType expr <<$>> parseExprInternal context (next p)
    parseInfix source = pure source

    parseRootExpr :: Parsed -> MaybeParse Expr
    parseRootExpr (EndParsed txt) = Left $ IncompleteExpression $ toTextRev txt
    parseRootExpr list@(CharParsed p@(old, x, xs))
      | isWhitespace x = parseRootExpr $ next p
      | x == '-' = (Valeur . NumValeur . (0 -)) <<$>> parseDigit (next p)
      | isAlpha x = parseText $ readText list
      | isDigit x = (Valeur . NumValeur) <<$>> parseDigit list
      | otherwise = Left $ UnrecognizedChar (toTextRev old) x (toText xs)
      where
        parseText :: PartialParse String -> MaybeParse Expr
        parseText (suite, "if") = do
          (suite1, ifexpr) <- parseExprInternal context suite
          (suite11, _) <- validate suite1 "then"
          (suite2, thenexpr) <- parseExprInternal context suite11
          (suite21, _) <- validate suite2 "else"
          (suite3, elseexpr) <- parseExprInternal context suite21
          pure (suite3, If ifexpr thenexpr elseexpr)
        parseText (suite, mot) = parseText1 (tryExtractParam context motT)
          where
            motT = toText mot

            parseText1 :: Maybe Int -> MaybeParse Expr
            parseText1 (Just val) = pure (suite, Parametre $ ParamIndex val)
            parseText1 Nothing =
              parseText2 (tryExtractFunction context $ toText motT)

            parseText2 :: Maybe Int -> MaybeParse Expr
            parseText2 Nothing =
              Left $ WordParseError (toTextRev old) motT (toText $ futurePart suite)
            parseText2 (Just params) =
              (\(suite2, exprs) -> (suite2, Fonction exprs (FonctionName motT))) <$> parseNExprs params suite mempty

            parseNExprs :: Int -> Parsed -> RevList Expr -> MaybeParse [Expr]
            parseNExprs 0 currTxt exprs = pure (currTxt, toListRev exprs)
            parseNExprs count currTxt exprs =
              parseExprInternal context currTxt >>= (\(suite3, expr) -> parseNExprs (count - 1) suite3 $ add expr exprs)

validate :: Parsed -> String -> MaybeParse ()
validate toparse test =
  let (suite, mot) = readText toparse
   in if mot == test then pure (suite, ()) else Left $ IncorrectWordError (toText test) (toText mot) (toTextRev $ parsedPart toparse) (toText $ futurePart suite)

parseDigit :: Parsed -> MaybeParse Int
parseDigit suite =
  let old = parsedPart suite
   in let remergeContext (a, b) = (merge a old, b)
       in remergeContext <$> parseDigitInternal old mempty (createParse $ futurePart suite)
  where
    parseDigitInternal :: RevString -> RevString -> Parsed -> MaybeParse Int
    parseDigitInternal old digits (CharParsed p@(_, x, _))
      | isDigit x = parseDigitInternal old (add x digits) $ next p
    parseDigitInternal old digits list =
      (list,) <$> either (\x -> Left $ (if x then IntParseError else IntOverflowError) (toTextRev old) list) pure (readMaybeInt $ toListRev digits)