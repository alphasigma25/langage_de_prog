{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


type Fonction = Expr
type Condition = Expr

data Expr
  = Valeur Int
  | Fonction [Expr] Fonction
  | Parametre Int
  | If Condition Expr Expr
  | Addition Expr Expr -- chaque nouvelle fonctionnalité = new constructeur
  | Multiplication Expr Expr
  | ReadFromConsole
  | PrintToConsole Expr
  | Error

evaluer :: [Int] -> Expr -> Int
evaluer context (Addition e1 e2) = evaluer context e1 + evaluer context e2
evaluer context (Multiplication e1 e2) = evaluer context e1 * evaluer context e2
evaluer _ (Valeur e) = e
evaluer context (Fonction p e2) = evaluer (fmap (evaluer context) p) e2
evaluer context (Parametre i) = context !! i
evaluer _ Error = error "Utilisation d'une instruction invalide"
evaluer context (If cond vrai faux) =
  if evaluer context cond /= 0 then evaluer context vrai else evaluer context faux
evaluer _ ReadFromConsole = 0
evaluer _ (PrintToConsole e) = 0

evaluer2 :: Expr -> IO Int
evaluer2 ReadFromConsole = pure 0
evaluer2 (PrintToConsole e) = print (evaluer [0] e) >>= const (pure 0)

evaluer3 :: Expr -> IO Int
evaluer3 (PrintToConsole e) =
    do
        x <- print (evaluer [0] e)
        const (pure 0) x

evaluer4 :: Expr -> IO Int
evaluer4 (PrintToConsole e) = fmap (const 0) (print (evaluer [0] e))

facths :: Int -> Int
facths n = if n /= 0 then n * facths n - 1 else 1

fact :: Fonction
fact = If
    (Parametre 0)
    (Multiplication
        (Parametre 0) $
        Fonction
            [Addition (Parametre 0) (Valeur (-1))]
            fact)
    (Valeur 1)
