module Expr where

newtype FonctionCode = FonctionCode Expr deriving (Show)

newtype FonctionName = FonctionName Text deriving (Show, Ord, Eq)

newtype FonctionArgCount = FonctionArgCount Int deriving (Show)

newtype ParamIndex = ParamIndex Int deriving (Show)

newtype NumValeur = NumValeur Int deriving (Show)

data Operation = OpAdd | OpSous | OpMult | OpDiv deriving (Show)

type Program = Map FonctionName (FonctionCode, FonctionArgCount)

type Condition = Expr

data Expr
  = Valeur NumValeur
  | Fonction [Expr] FonctionName
  | Parametre ParamIndex
  | If Condition Expr Expr
  | Operation Operation Expr Expr
  | Error
  deriving (Show)