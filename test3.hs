import Data.Char (isDigit)
import Data.Maybe (catMaybes, fromMaybe)

type Fonction = Expr

type Condition = Expr

-- fmap' :: m a -> (a -> b) -> m b
-- apply' :: m a -> m (a -> b) -> m b
-- apply'' :: m a -> m b -> (a -> b -> c) -> m c
-- bind' :: m a -> (a -> m b) -> m b

data Expr
  = Valeur Int
  | Fonction [Expr] Fonction
  | Parametre Int
  | If Condition Expr Expr
  | Addition Expr Expr -- chaque nouvelle fonctionnalité = new constructeur
  | Multiplication Expr Expr
  | Error
  deriving (Show)

-- Comment on fait pour avoir un parse qui évite les overflow et les caractères invalides
safeParse :: String -> Maybe Int
safeParse str = Just $ read str

data Etat = Initial | IntParse String | Noeud (Expr -> Expr) Etat

parser :: String -> Maybe Expr
parser s = internalParse s Initial

-- Est ce que c'est un problème d'avoir des noeuds imbriqués (parseChar
-- doit déplier tout les noeuds pour chaque char)
internalParse :: String -> Etat -> Maybe Expr
internalParse [] Initial = Nothing
internalParse [] (IntParse int_str) = Valeur <$> safeParse int_str
internalParse [] (Noeud incExpr e) = incExpr <$> internalParse [] e
internalParse (x : xs) e = parseChar e x >>= internalParse xs

parseChar :: Etat -> Char -> Maybe Etat
parseChar Initial c
  | isDigit c = Just $ IntParse [c]
  | c == ' ' = Just Initial
  | otherwise = Nothing
parseChar (IntParse e) c
  | isDigit c = Just $ IntParse (e ++ [c])
  | c == '+' = (\x -> Noeud (Addition $ Valeur x) Initial) <$> safeParse e
  | c == '*' = (\x -> Noeud (Multiplication $ Valeur x) Initial) <$> safeParse e
  | otherwise = Nothing
parseChar (Noeud incExpr e) c = Noeud incExpr <$> parseChar e c

test :: Maybe Expr
test = parser "12+35"

evaluer :: [Int] -> Expr -> Int
evaluer context (Addition e1 e2) = evaluer context e1 + evaluer context e2
evaluer context (Multiplication e1 e2) = evaluer context e1 * evaluer context e2
evaluer _ (Valeur e) = e
evaluer context (Fonction p e2) = evaluer (fmap (evaluer context) p) e2
evaluer context (Parametre i) = context !! i
evaluer _ Error = error "Utilisation d'une instruction invalide"
evaluer context (If cond vrai faux) =
  if evaluer context cond /= 0 then evaluer context vrai else evaluer context faux

facths :: Int -> Int
facths n = if n /= 0 then n * facths n - 1 else 1

fact :: Fonction
fact =
  If
    (Parametre 0)
    ( Multiplication
        (Parametre 0)
        $ Fonction
          [Addition (Parametre 0) (Valeur (-1))]
          fact
    )
    (Valeur 1)
