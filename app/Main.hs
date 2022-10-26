module Main where

import Data.Map as M (empty)
import Eval (evaluer)
import Expr
import Parser (parseExpr, parseFunc)
import ParsingError (ParseError, notAFuncError)

main :: IO ()
main = internalMain M.empty
  where
    internalMain :: Program -> IO ()
    internalMain ct = do
      putStr "> "
      hFlush stdout
      line <- getLine
      let (execution, ctx) = printResult $ tryParseLine (toString line)
      execution
      internalMain ctx
      where
        printResult :: Either ParseError (Either Program Expr) -> (IO (), Program)
        printResult (Right (Left prg)) = (pass, prg)
        printResult (Right (Right expr)) = (print (evaluer expr ct), ct)
        printResult (Left err) = (print err, ct)

        tryParseLine :: String -> Either ParseError (Either Program Expr)
        tryParseLine line = tryParseExpr (parseFunc ct line)
          where
            tryParseExpr :: Either ParseError Program -> Either ParseError (Either Program Expr)
            tryParseExpr (Right x) = pure $ Left x
            tryParseExpr (Left x)
              | notAFuncError x = pure <$> parseExpr ct line
              | otherwise = Left x

facths :: Int -> Int
facths n = if n /= 0 then n * facths n - 1 else 1

fact :: FonctionCode
fact =
  FonctionCode $
    If
      (Parametre $ ParamIndex 0)
      ( Operation
          OpMult
          (Parametre $ ParamIndex 0)
          $ Fonction
            [Operation OpAdd (Parametre $ ParamIndex 0) (Valeur (NumValeur $ -1))]
            (FonctionName $ toText "fact")
      )
      (Valeur $ NumValeur 1)

factSource :: String
factSource = "fact x = if x then x * fact x - 1 else 1. fact 10"