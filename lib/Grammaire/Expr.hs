module Expr where

type Fonction = Expr

type Condition = Expr

data Operation = Addition | Soustration | Multiplication | Division deriving Show

data Expr
  = Valeur Int
  | Fonction [Expr] Fonction
  | Parametre Int
  | If Condition Expr Expr
  -- | Addition Expr Expr -- chaque nouvelle fonctionnalité = new constructeur
  -- | Multiplication Expr Expr
  | Operation Operation Expr Expr
  | Error
  deriving (Show)