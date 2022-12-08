module ExprToLogical (exprToLogicalInstr, LogicalInstr (..), LogReg) where

import CompileError (CompileError (UndefError))
import Data.Int (Int16)
import Data.Map (Map)
import Expr (Expr (..), FctDef, Op)
import RegToBinary (Src1 (Constant, Registre))

type LogReg = Integer

data LogicalInstr
  = STOP (Src1 LogReg)
  | CONST (Src1 LogReg) LogReg
  | OP Op (Src1 LogReg) LogReg LogReg

exprToLogicalInstr :: Map String FctDef -> Either CompileError (Map String ([LogicalInstr], LogReg))
exprToLogicalInstr = mapM $ exprToInstr 0

exprToInstr :: LogReg -> (Int16, Expr) -> Either CompileError ([LogicalInstr], LogReg)
exprToInstr target (_, Valeur v) = Right ([CONST (Constant v) target], target)
exprToInstr _ (_, Call {}) = undefined -- Pas gentil
exprToInstr _ (_, ParamDef _) = undefined -- Pas gentil
exprToInstr _ (_, If {}) = undefined -- Pas gentil
exprToInstr target (nbParams, Operation op exp1 exp2) =
  do
    (instrE1, t1) <- exprToInstr target (nbParams, exp1)
    (instrE2, t2) <- exprToInstr (t1 + 1) (nbParams, exp2)
    pure (instrE1 ++ instrE2 ++ [OP op (Registre t1) t2 $ t2 + 1], t2 + 1)
exprToInstr _ (_, Undefined) = Left UndefError