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
  | Fonction String [Expr]
  | Parametre Int
  | If Condition Expr Expr
  | -- | Addition Expr Expr -- chaque nouvelle fonctionnalité = new constructeur
    -- | Multiplication Expr Expr --TODO
    Operation Operation Expr Expr
  | Undefined

instance Show Expr where
  show :: Expr -> String
  show (Valeur v) = "value " ++ show v  -- TODO ?
  show (Parametre p) = "param " ++ show p
  show (Operation op v1 v2) = show v1 ++ ' ' : show op ++ ' ' : show v2
  show Undefined = "???"
  show (Fonction _ _) = error "TODO" -- TODO
  show (If cond true false) = "if " ++ show cond ++ " then " ++ show true ++ " else " ++ show false