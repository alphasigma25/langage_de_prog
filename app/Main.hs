module Main where

import Eval.Eval (evaluer)
import Grammaire.Expr
import Parser.Parser (parser)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  line <- getLine
  case parser line of
    Left err -> print err
    Right ex -> do
      print ex
      print $ evaluer [] ex
  main

facths :: Int -> Int
facths n = if n /= 0 then n * facths n - 1 else 1

fact :: Fonction
fact =
  If
    (Parametre 0)
    ( Multiplication
        (Parametre 0)
        $ Fonction
          [Addition (Parametre 0) (Valeur (-1))]
          fact
    )
    (Valeur 1)