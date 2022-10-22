module Main where

import Eval.Eval (evaluerProg)
import Grammaire.Expr
import Parser.Parser (parser)

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  line <- getLine
  case parser line of
    Left err -> print err
    Right ex -> do
      print ex
      print $ evaluerProg ex
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
          0
    )
    (Valeur 1)