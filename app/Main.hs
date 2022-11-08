module Main where

import Data.Map (Map, empty, insert)
import Eval (FctDef, evaluer)
import Expr (Expr (..), Operation (..))
import Parser (parseRepl)
import System.IO (hFlush, stdout)
-- type FctDef = (Int, Expr)
main :: IO ()
main = mainInternal empty
  where
    mainInternal :: Map String FctDef -> IO ()
    mainInternal context = do
      putStr "> "
      hFlush stdout
      line <- getLine
      case parseRepl (fmap fst context) line of
        Left err -> print err >> mainInternal context
        Right (Right (name, fctDef)) -> mainInternal $ insert name fctDef context
        Right (Left ex) -> do
          print ex -- TODO ?
          print $ evaluer context ex
          mainInternal context

facths :: Int -> Int
facths n = if n /= 0 then n * facths n - 1 else 1

fact :: Expr
fact =
  If
    (ParamDef 0)
    ( Operation
        Multiplication
        (ParamDef 0)
        $ Call
          "fact"
          [Operation Addition (Call "fact" []) (Valeur (-1))]
    )
    (Valeur 1)