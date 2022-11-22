module Main where

import Data.Foldable (foldl')
import Data.Map (Map, empty, insert, (!?))
import Eval (FctDef, evaluer)
import Expr (Expr (..), Operation (..))
import Parser (ParseError, parseRepl, parserFile, parserReplExpr)
import System.IO (Handle, IOMode (ReadMode), hFlush, hGetContents, stdout, withFile)

-- type FctDef = (Int, Expr)
main :: IO ()
main = mainInternal empty
  where
    mainInternal :: Map String FctDef -> IO ()
    mainInternal context = do
      putStr "> "
      hFlush stdout
      line <- getLine
      getCommand line
      where
        getCommand :: String -> IO () --TODO : ajouter une commande pour effacer le contenu du REPL
        getCommand ":q" = pure ()
        getCommand (':' : 'l' : ' ' : filename) =
          let a = withFile filename ReadMode parseFile in a >>= mainInternal
          where
            parseFile :: Handle -> IO (Map String FctDef)
            parseFile h = hGetContents h >>= integrateInContext . parserFile

            integrateInContext :: Either ParseError [(String, FctDef)] -> IO (Map String FctDef)
            integrateInContext (Left err) = context <$ print err
            integrateInContext (Right defs) = pure $ foldl' (\acc (name, fdef) -> insert name fdef acc) context defs
        getCommand (':' : 'd' : 'e' : ' ' : xs) =
          either print print (parserReplExpr (fmap fst context) xs) >> mainInternal context
        getCommand (':' : 'd' : 'f' : ' ' : xs) =
          maybe (print $ "no function found with name " ++ xs) (\(nparams, ex) -> putStrLn $ xs ++ (concat [' ' : '*' : 'p' : show x | x <- [0 .. (nparams -1)]]) ++ " = " ++ show ex) (context !? xs) >> mainInternal context
        getCommand (':' : xs) = putStrLn ("Undefined command " ++ xs) >> mainInternal context
        getCommand "" = mainInternal context
        getCommand line = case parseRepl (fmap fst context) line of
          Left err -> print err >> mainInternal context
          Right (Right (name, fctDef)) -> mainInternal $ insert name fctDef context
          Right (Left ex) -> print (evaluer context ex) >> mainInternal context

file :: String
file = "f a b = a + b . g a b = a * b"

file2 :: String
file2 = "f a b = a + b . f a b = a * b"

facths :: Int -> Int
facths n = if n /= 0 then n * facths n - 1 else 1

fact :: Expr
fact =
  If
    (ParamDef 0)
    ( Operation
        Multiplication
        (ParamDef 0)
        $ Call
          "fact"
          [Operation Addition (Call "fact" []) (Valeur (-1))]
    )
    (Valeur 1)