{-# LANGUAGE InstanceSigs #-}

module Eval (RuntimeError, evaluer, FctDef) where

import Data.List.Extra as L ((!?))
import Data.Map as M (Map, (!?))
import Expr (Expr (..), FctDef, Operation (..))

data RuntimeError
  = RTError
  | UnknownExprError
  | UnknownFunction
  | InvalidParams Int Int
  | ZeroDiv

instance Show RuntimeError where
  show :: RuntimeError -> String
  show RTError = "Param out of range"
  show UnknownFunction = "Unknown function name"
  show UnknownExprError = "Invalid expression"
  show ZeroDiv = "Division by zero"
  show (InvalidParams expected actual) = "Invalid number of parameters, expected : " ++ show expected ++ " actual : " ++ show actual

applyOp :: Operation -> Int -> Int -> Either RuntimeError Int
applyOp Addition x y = Right $ x + y
applyOp Soustration x y = Right $ x - y
applyOp Multiplication x y = Right $ x * y
applyOp Division _ 0 = Left ZeroDiv
applyOp Division x y = Right $ div x y

type Context = (Map String FctDef, [Int]) --  nom_fct -> (nb de param, expr), [valeurs params]

evaluer :: Map String FctDef -> Expr -> Either RuntimeError Int
evaluer m = evaluerInternal (m, [])
  where
    evaluerInternal :: Context -> Expr -> Either RuntimeError Int
    evaluerInternal context (Operation op e1 e2) = do
      ex1 <- evaluerInternal context e1
      ex2 <- evaluerInternal context e2
      applyOp op ex1 ex2
    evaluerInternal _ (Valeur e) = Right e
    evaluerInternal ctx@(fcts, _) (Call name exprs) = do
      fctExpr <- maybe (Left UnknownFunction) checkLength (fcts M.!? name)
      fctParams <- traverse (evaluerInternal ctx) exprs
      evaluerInternal (fcts, fctParams) fctExpr
      where
        checkLength (nbParams, expr) = if nbParams == length exprs then Right expr else Left $ InvalidParams nbParams $ length exprs
    evaluerInternal (_, params) (ParamDef i) = maybe (Left RTError) Right (params L.!? i)
    evaluerInternal _ Undefined = Left UnknownExprError
    evaluerInternal context (If cond vrai faux) =
      evaluerInternal context cond >>= (\x -> evaluerInternal context $ if x /= 0 then vrai else faux)

-- a x b = x + b
-- ("a", (2, (Add (ParamDef 0) (ParamDef 1))))
-- main = a 5 6
-- ("main", (Call "a" [(Valeur 5), (Valeur 6)], 0))

-- Pour monad => fmap, apply, bind
-- Pour list => fmap, fold, traverse
-- fmap (a -> b) -> [a] -> [b]
-- traverse (a -> m b) [a] -> m [b]