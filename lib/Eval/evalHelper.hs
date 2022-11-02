module EvalHelper where

import Expr (NumValeur (..), Operation (..), ParamIndex (..), Program)
import RuntimeError (RuntimeError (..))

newtype ParamList = ParamList [NumValeur]

type Context = (ParamList, Program)

readParam :: ParamList -> ParamIndex -> Either RuntimeError NumValeur
readParam (ParamList m) (ParamIndex p) = maybeToRight InvalidParamIndex $ m !!? p

check :: NumValeur -> Bool
check (NumValeur v) = v /= 0

evaluerOp :: Operation -> NumValeur -> NumValeur -> Either RuntimeError NumValeur
evaluerOp OpAdd (NumValeur e1) (NumValeur e2) = pure $ NumValeur $ e1 + e2
evaluerOp OpSous (NumValeur e1) (NumValeur e2) = pure $ NumValeur $ e1 - e2
evaluerOp OpMult (NumValeur e1) (NumValeur e2) = pure $ NumValeur $ e1 * e2
evaluerOp OpDiv _ (NumValeur 0) = Left DivByZero
evaluerOp OpDiv (NumValeur e1) (NumValeur e2) = pure $ NumValeur $ div e1 e2