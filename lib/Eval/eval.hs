module Eval (evaluer) where

import Data.Map as M ((!?))
import EvalHelper (Context, ParamList (ParamList), check, evaluerOp, readParam)
import Expr

evaluer :: Expr -> Program -> Maybe NumValeur
evaluer expr ctx = internalEvaluer expr (ParamList mempty, ctx)
  where
    internalEvaluer :: Expr -> Context -> Maybe NumValeur
    internalEvaluer (Operation op e1 e2) context = do
      v1 <- internalEvaluer e1 context
      v2 <- internalEvaluer e2 context
      evaluerOp op v1 v2
    internalEvaluer (Valeur e) _ = pure e
    internalEvaluer (Parametre i) (params, _) = readParam params i
    internalEvaluer Error _ = Nothing
    internalEvaluer (If cond vrai faux) context =
      internalEvaluer cond context >>= (\test -> internalEvaluer (if check test then vrai else faux) context)
    internalEvaluer (Fonction p e2) context@(_, funcs) =
      do
        func <- funcs M.!? e2
        param <- traverse (`internalEvaluer` context) p
        evaluerFunc func param
      where
        evaluerFunc :: (FonctionCode, FonctionArgCount) -> [NumValeur] -> Maybe NumValeur
        evaluerFunc (FonctionCode code, FonctionArgCount count) param = checkValidLenght >> internalEvaluer code (ParamList param, funcs)
          where
            checkValidLenght :: Maybe ()
            checkValidLenght = when (length param /= count) Nothing