module Eval.Eval where

import Grammaire.Expr

type Context = ([Int], [Fonction])

evaluerProg :: Program -> Maybe Int
evaluerProg list@(x:_) = evaluer x ([], list)
evaluerProg [] = Nothing

evaluer :: Expr -> Context -> Maybe Int
evaluer (Addition e1 e2) context = (+) <$> evaluer e1 context <*> evaluer e2 context
evaluer (Multiplication e1 e2) context = (*) <$> evaluer e1 context <*> evaluer e2 context
evaluer (Valeur e) _ = Just e
evaluer (Fonction p e2) context@(_, funcs) =
  ((,) <$> tryGetElem e2 funcs <*> traverse (`evaluer` context) p) >>= (\(x, y) -> evaluer x (y, funcs))
evaluer (Parametre i) (params, _) = tryGetElem i params
evaluer Error _ = error $ toText "Utilisation d'une instruction invalide"
evaluer (If cond vrai faux) context =
  evaluer cond context >>= (\test -> evaluer (if test /= 0 then vrai else faux) context)

tryGetElem :: Int -> [a] -> Maybe a
tryGetElem _ [] = Nothing
tryGetElem 0 (x : _) = Just x
tryGetElem p (_ : xs) = tryGetElem (p - 1) xs