module Main where

import Control.Monad (foldM)
import Data.Map (Map, empty, insert, member, (!?))
import Eval (FctDef, evaluer)
import Parser (ParseError, parseRepl, parserFile, parserReplExpr)
import System.IO (Handle, IOMode (ReadMode), hFlush, hGetContents, stdout, withFile)

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
        getCommand :: String -> IO ()
        getCommand ":q" = pure ()
        getCommand ":del" = main
        getCommand (':' : 'l' : ' ' : filename) =
          let a = withFile filename ReadMode parseFile in a >>= mainInternal
          where
            parseFile :: Handle -> IO (Map String FctDef)
            parseFile h = hGetContents h >>= integrateInContext . parserFile

            integrateInContext :: Either ParseError [(String, FctDef)] -> IO (Map String FctDef)
            integrateInContext (Left err) = context <$ print err
            integrateInContext (Right defs) =
              let mcontext =
                    foldM
                      ( \acc (name, fdef) ->
                          if member name acc then Left name else pure $ insert name fdef acc
                      )
                      context
                      defs
               in either
                    (\nameErr -> context <$ print ("Function name collision between file and REPL context : " ++ nameErr))
                    pure
                    mcontext
        getCommand (':' : 'd' : 'e' : 'x' : 'p' : 'r' : ' ' : xs) =
          either print print (parserReplExpr (fmap fst context) xs) >> mainInternal context
        getCommand (':' : 'd' : 'e' : 'f' : ' ' : xs) =
          maybe (print $ "no function found with name " ++ xs) (\(nparams, ex) -> putStrLn $ xs ++ (concat [' ' : '*' : 'p' : show x | x <- [0 .. (nparams -1)]]) ++ " = " ++ show ex) (context !? xs) >> mainInternal context
        getCommand (':' : xs) = putStrLn ("Undefined command " ++ xs) >> mainInternal context
        getCommand "" = mainInternal context
        getCommand line = case parseRepl (fmap fst context) line of
          Left err -> print err >> mainInternal context
          Right (Right (name, fctDef)) -> mainInternal $ insert name fctDef context
          Right (Left ex) -> print (evaluer context ex) >> mainInternal context