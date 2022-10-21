module Eval.Eval where

import Grammaire.Expr

evaluer :: Expr -> [Int] -> Maybe Int
evaluer (Addition e1 e2) context = (+) <$> evaluer e1 context <*> evaluer e2 context
evaluer (Multiplication e1 e2) context = (*) <$> evaluer e1 context <*> evaluer e2 context
evaluer (Valeur e) _ = Just e
evaluer (Fonction p e2) context = traverse (`evaluer` context) p >>= evaluer e2
evaluer (Parametre i) context = Just $ context !! i
evaluer Error _ = error "Utilisation d'une instruction invalide"
evaluer (If cond vrai faux) context =
  evaluer cond context >>= (\test -> evaluer (if test /= 0 then vrai else faux) context)