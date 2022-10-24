{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser.Parser where

import Grammaire.Expr ( Program )
import Parser.ExpressionParser ( parseExpr )
import Parser.Zip ( createParse, incomplete )
import Parser.CommonParser ( ParseError(OverflowExpression) )

parser :: Text -> Either ParseError Program
parser list =
  parseExpr (createParse $ toString list)
    >>= (\(text, result) -> if incomplete text then Left $ OverflowExpression text else Right [result])