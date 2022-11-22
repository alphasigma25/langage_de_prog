{-# LANGUAGE InstanceSigs #-}

module Expr where

type Condition = Expr

type FctDef = (Int, Expr)

data Operation = Addition | Soustration | Multiplication | Division

instance Show Operation where
  show :: Operation -> String
  show Addition = "+"
  show Soustration = "-"
  show Multiplication = "*"
  show Division = "/"

data Expr
  = Valeur Int
  | Call String [Expr]
  | ParamDef Int -- No du param dans la fonction
  | If Condition Expr Expr
  | Operation Operation Expr Expr
  | Undefined

instance Show Expr where
  show :: Expr -> String
  show (Valeur v) = show v
  show (ParamDef p) = "*p" ++ show p
  show (Operation op v1 v2) = show v1 ++ ' ' : show op ++ ' ' : show v2
  show Undefined = "???"
  show (Call name exprs) = name ++ foldl (\acc y -> acc ++ ' ' : show y) "" exprs
  show (If cond true false) = "if " ++ show cond ++ " then " ++ show true ++ " else " ++ show false