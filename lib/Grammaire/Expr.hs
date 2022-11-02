{-# LANGUAGE InstanceSigs #-}

module Expr where

import qualified Text.Show

newtype FonctionCode = FonctionCode Expr

newtype FonctionName = FonctionName Text deriving (Ord, Eq)

newtype FonctionArgCount = FonctionArgCount Int

newtype ParamIndex = ParamIndex Int

newtype NumValeur = NumValeur Int

data Operation = OpAdd | OpSous | OpMult | OpDiv

type Program = Map FonctionName (FonctionCode, FonctionArgCount)

type Condition = Expr

data Expr
  = Valeur NumValeur
  | Fonction [Expr] FonctionName
  | Parametre ParamIndex
  | If Condition Expr Expr
  | Operation Operation Expr Expr
  | Error

instance Show NumValeur where
  show :: NumValeur -> String
  show (NumValeur v) = show v

instance Show FonctionCode where
  show :: FonctionCode -> String
  show (FonctionCode fc) = show fc

instance Show Operation where
  show :: Operation -> String
  show OpAdd = "+"
  show OpSous = "-"
  show OpMult = "*"
  show OpDiv = "/"

instance Show Expr where
  show :: Expr -> String
  show Error = "Error"
  show (If cond expr1 expr2) = "If " ++ show cond ++ " then " ++ show expr1 ++ " else " ++ show expr2
  show (Valeur c) = show c
  show (Parametre (ParamIndex p)) = 'p' : show p
  show (Fonction params (FonctionName name)) = toString name ++ foldl' (\x y -> x ++ ' ' : show y) "" params
  show (Operation op v1 v2) = show v1 ++ ' ' : show op ++ ' ' : show v2