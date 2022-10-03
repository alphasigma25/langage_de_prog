{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Maybe (fromMaybe)

type Fonction = Expr

type Condition = Expr

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

parser :: String -> Either String (Expr, String)
parser [] = Left []
parser list@(c : cs)
  | isDigit c =
    let num_suite = parseNum list
     in Right $ tryExtend (Valeur $ fst num_suite) $ snd num_suite
  | c == ' ' = parser cs
  | isAlpha c = uncurry tryExtend <$> uncurry treatText (parseText list)
  | otherwise = Left list

-- on cherche à étendre l'expression avant de rentrer dans une nouvelle expression
tryExtend :: Expr -> String -> (Expr, String)
tryExtend e [] = (e, [])
tryExtend e list@(c : cs)
  | c == ' ' = tryExtend e cs
  | c == '+' =
    let joker x y = (Addition e x, y)
     in extractEither (e, list) (uncurry joker) (parser cs)
  | c == '*' =
    let joker x y = (Multiplication e x, y)
     in extractEither (e, list) (uncurry joker) (parser cs)
  | otherwise = (e, list)

extractEither :: t -> (b -> t) -> Either a b -> t
extractEither defaultVal transformer (Right x) = transformer x
extractEither defaultVal transformer (Left x) = defaultVal

treatText :: String -> String -> Either String (Expr, String)
treatText "if" suite = parseIf suite
treatText txt suite = Left suite

parseIf :: String -> Either String (Expr, String)
parseIf str =
  do
    cond <- parser str
    thenBloc <- validateText "then" (parseText $ snd cond)
    vrai <- parser thenBloc
    elseBloc <- validateText "else" (parseText $ snd vrai)
    faux <- parser elseBloc
    pure (If (fst cond) (fst vrai) (fst faux), snd faux)

validateText :: String -> (String, String) -> Either String String
validateText str1 str2 = if str1 == fst str2 then Right $ snd str2 else Left $ snd str2

type NumState = String

parseNum :: String -> (Int, String)
parseNum = parseNumInternal ""

parseNumInternal :: NumState -> String -> (Int, String)
parseNumInternal state list@(x : xs)
  | isDigit x = parseNumInternal (state ++ [x]) xs
  | otherwise = (read state, list)
parseNumInternal state [] = (read state, [])

type TextState = String

parseText :: String -> (String, String)
parseText = parseTextInternal ""

parseTextInternal :: NumState -> String -> (String, String)
parseTextInternal state list@(x : xs)
  | isAlphaNum x = parseTextInternal (state ++ [x]) xs
  | otherwise = (state, list)
parseTextInternal state [] = (state, [])

test = "if 0 + 5 then 2 + 3 else 5+ 4" -- +if 7 *8 then7 else 8"

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
