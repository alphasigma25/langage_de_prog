module Grammaire.Expr where

type Fonction = Expr

type Condition = Expr

data Expr
  = Valeur Int
  | Fonction [Expr] Fonction
  | Parametre Int
  | If Condition Expr Expr
  | Addition Expr Expr -- chaque nouvelle fonctionnalité = new constructeur
  | Multiplication Expr Expr
  | Error
  deriving (Show)