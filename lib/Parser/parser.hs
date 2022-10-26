{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser (parseExpr, parseFunc) where

import Context (parsingContextFromProg)
import Data.Map (insert, size)
import Expr (Expr, FonctionArgCount (FonctionArgCount), FonctionCode (FonctionCode), Program)
import ExpressionParser (ParsingContext (ParsingContext), parseExprInternal)
import FonctionParser (ParsedFonctionHeader, ParsedHeaderResult, parseFonction)
import ParsingError (ParseError (OverflowExpression))
import Zip (createParse, incomplete)

parseFunc :: Program -> String -> Either ParseError Program
parseFunc prg str = parseFonction (parsingContextFromProg prg) (createParse str) >>= mergesInContext
  where
    mergesInContext :: ParsedHeaderResult -> Either ParseError Program
    mergesInContext (ctx, list) = foldl' mergeInContext (pure prg) list
      where
        mergeInContext :: Either ParseError Program -> ParsedFonctionHeader -> Either ParseError Program
        mergeInContext err@(Left _) _ = err
        mergeInContext (Right prog) (name, params, code) =
          (\x -> insert name (FonctionCode x, FonctionArgCount $ size params) prog)
            <$> parseExpr2 (ParsingContext (ctx, params)) code

parseExpr :: Program -> String -> Either ParseError Expr
parseExpr ctx = parseExpr2 (ParsingContext (parsingContextFromProg ctx, mempty))

parseExpr2 :: ParsingContext -> String -> Either ParseError Expr
parseExpr2 ctx str =
  parseExprInternal
    ctx
    (createParse str)
    >>= (\(text, result) -> if incomplete text then Left $ OverflowExpression text else pure result)