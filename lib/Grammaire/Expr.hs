{-# LANGUAGE InstanceSigs #-}

module Expr where

import Data.Int (Int16)
import Data.Map (Map, size)

type Condition = Expr

type FctDef = (Int16, Expr)

data Op = Add | Sub | Mul | Div | Mod | RSub | RDiv | RMod

instance Show Op where
  show :: Op -> String
  show Add = "+"
  show Sub = "-"
  show RSub = "rsub"
  show Mul = "*"
  show Div = "/"
  show RDiv = "rdiv"
  show Mod = "%"
  show RMod = "rmod"

data Expr
  = Valeur Int16
  | Call String [Expr]
  | ParamDef Int16 -- No du param dans la fonction
  | If Condition Expr Expr
  | Operation Op Expr Expr
  | Undefined

instance Show Expr where
  show :: Expr -> String
  show (Valeur v) = show v
  show (ParamDef p) = "*p" ++ show p
  show (Operation op v1 v2) = show v1 ++ ' ' : show op ++ ' ' : show v2
  show Undefined = "???"
  show (Call name exprs) = name ++ foldl (\acc y -> acc ++ ' ' : show y) "" exprs
  show (If cond true false) = "if " ++ show cond ++ " then " ++ show true ++ " else " ++ show false

int16ToInt :: Int16 -> Int
int16ToInt = fromIntegral

length16 :: [a] -> Int16
length16 = fromIntegral . length

size16 :: Map k v -> Int16
size16 = fromIntegral . size