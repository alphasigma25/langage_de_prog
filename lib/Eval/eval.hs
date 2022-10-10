module Eval.Eval where
import Grammaire.Expr

evaluer :: [Int] -> Expr -> Int
evaluer context (Addition e1 e2) = evaluer context e1 + evaluer context e2
evaluer context (Multiplication e1 e2) = evaluer context e1 * evaluer context e2
evaluer _ (Valeur e) = e
evaluer context (Fonction p e2) = evaluer (fmap (evaluer context) p) e2
evaluer context (Parametre i) = context !! i
evaluer _ Error = error "Utilisation d'une instruction invalide"
evaluer context (If cond vrai faux) =
  if evaluer context cond /= 0 then evaluer context vrai else evaluer context faux
