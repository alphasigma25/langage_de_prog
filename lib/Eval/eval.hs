module Eval.Eval where

import Grammaire.Expr

evaluer :: Expr -> [Int] -> Maybe Int
evaluer (Addition e1 e2) context = (+) <$> evaluer e1 context <*> evaluer e2 context
evaluer (Multiplication e1 e2) context = (*) <$> evaluer e1 context <*> evaluer e2 context
evaluer (Valeur e) _ = Just e
evaluer (Fonction p e2) context = traverse (`evaluer` context) p >>= evaluer e2
evaluer (Parametre i) context = tryGetParam i context
evaluer Error _ = error $ toText "Utilisation d'une instruction invalide"
evaluer (If cond vrai faux) context =
  evaluer cond context >>= (\test -> evaluer (if test /= 0 then vrai else faux) context)

tryGetParam :: Int -> [Int] -> Maybe Int
tryGetParam _ [] = Nothing
tryGetParam 0 (x : _) = Just x
tryGetParam p (_ : xs) = tryGetParam (p - 1) xs