module Eval (RuntimeError, evaluer) where

import Expr
import Data.List.Extra( (!?) )

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

evaluer :: [Int] -> Expr -> Either RuntimeError Int
evaluer context (Operation op e1 e2) = do
    ex1 <- evaluer context e1
    ex2 <- evaluer context e2
    applyOp op ex1 ex2
evaluer _ (Valeur e) = Right e
-- evaluer context (Fonction p e2) = evaluer (fmap (evaluer context) p) e2
evaluer _ (Fonction _ _) = undefined
evaluer context (Parametre i) = maybe (Left RTError) Right (context !? i)
evaluer _ Error = Left UnknownExprError
evaluer context (If cond vrai faux) =
  evaluer context cond >>= (\x -> evaluer context $ if x /= 0 then vrai else faux)