module ExprToLogical (exprToLogicalInstr, LogicalInstr (..), LogReg) where

import CompileError (CompileError (UndefError))
import Data.Int (Int16)
import Data.Map (Map)
import Expr (Expr (..), FctDef, Op)
import RegToBinary (Src1 (Constant, Registre))

newtype LogReg = LogReg Integer

instance Show LogReg where
  show (LogReg val) = "R" ++ show val

initLogReg :: LogReg
initLogReg = LogReg 0

nextLR :: LogReg -> LogReg
nextLR (LogReg val) = LogReg $ val + 1

data LogicalInstr
  = STOP (Src1 LogReg)
  | CONST (Src1 LogReg) LogReg
  | OP Op (Src1 LogReg) LogReg LogReg

instance Show LogicalInstr where
  show (STOP val) = "STOP " ++ show val
  show (CONST cst t) = show t ++ " <- CONST " ++ show cst
  show (OP opCode op1 op2 t) = show t ++ " <- " ++ show op1 ++ ' ' : show opCode ++ ' ' : show op2

exprToLogicalInstr :: Map String FctDef -> Either CompileError (Map String ([LogicalInstr], LogReg))
exprToLogicalInstr = mapM $ exprToInstr initLogReg

exprToInstr :: LogReg -> (Int16, Expr) -> Either CompileError ([LogicalInstr], LogReg)
exprToInstr target (_, Valeur v) = Right ([CONST (Constant v) target], target)
exprToInstr _ (_, Call {}) = undefined -- Pas gentil
exprToInstr _ (_, ParamDef _) = undefined -- Pas gentil
exprToInstr _ (_, If {}) = undefined -- Pas gentil
exprToInstr target (nbParams, Operation op exp1 exp2) =
  do
    (instrE1, t1) <- exprToInstr target (nbParams, exp1)
    (instrE2, t2) <- exprToInstr (nextLR t1) (nbParams, exp2)
    pure (instrE1 ++ instrE2 ++ [OP op (Registre t1) t2 $ nextLR t2], nextLR t2)
exprToInstr _ (_, Undefined) = Left UndefError