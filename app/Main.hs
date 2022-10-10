module Main where

import Grammaire.Expr
  ( Expr (Addition, Fonction, If, Multiplication, Parametre, Valeur),
    Fonction,
  )
import Parser.Parser

main :: IO ()
main = do
  print $ parser "1 + 2"
  pure ()

-- test :: [Char]
-- test = "if 0 + 5 then 2 + 3 else 5+ 4" -- +if 7 *8 then7 else 8"

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