module Eval (RuntimeError, evaluer, FctDef) where

import Data.List.Extra ((!?))
import Data.Map (Map)
import Expr (Expr (..), FctDef, Operation (..))

data RuntimeError
  = RTError
  | UnknownExprError
  | ZeroDiv

instance Show RuntimeError where
  show RTError = "Param out of range"
  show UnknownExprError = "Invalid expression"
  show ZeroDiv = "Division by zero"

applyOp :: Operation -> Int -> Int -> Either RuntimeError Int
applyOp Addition x y = Right $ x + y
applyOp Soustration x y = Right $ x - y
applyOp Multiplication x y = Right $ x * y
applyOp Division _ 0 = Left ZeroDiv
applyOp Division x y = Right $ div x y

type Context = (Map String FctDef, [Int])

evaluer :: Map String FctDef -> Expr -> Either RuntimeError Int
evaluer m = evaluerInternal (m, [])
  where
    evaluerInternal :: Context -> Expr -> Either RuntimeError Int
    evaluerInternal context (Operation op e1 e2) = do
      ex1 <- evaluerInternal context e1
      ex2 <- evaluerInternal context e2
      applyOp op ex1 ex2
    evaluerInternal _ (Valeur e) = Right e
    -- evaluer (fct, params) (Fonction p e2) = evaluer (fmap (evaluer context) p) e2 --TODO
    evaluerInternal _ (Fonction _ _) = undefined --TODO
    evaluerInternal (_, params) (Parametre i) = maybe (Left RTError) Right (params !? i)
    evaluerInternal _ Undefined = Left UnknownExprError
    evaluerInternal context (If cond vrai faux) =
      evaluerInternal context cond >>= (\x -> evaluerInternal context $ if x /= 0 then vrai else faux)