{-# LANGUAGE BinaryLiterals #-}

module Main where

import CompileError (CompileError)
import Data.ByteString.Builder (hPutBuilder)
import Data.Map (Map, fromList, foldrWithKey)
import Expr (FctDef)
import ExprToLogical (LogicalInstr, exprToLogicalInstr, LogReg)
import Parser (ParseError, parserFile)
import RegToBinary (Instruction (..), Reg, Src1 (Constant), programToBin)
import Register8 (Reg8)
import System.IO (Handle, IOMode (ReadMode, WriteMode), hGetContents, withFile)
import Data.Foldable (foldl')

writeInstr :: Reg regType => [Instruction regType] -> IO ()
writeInstr prog = withFile "result.asc" WriteMode (`hPutBuilder` programToBin prog)

parseFile :: String -> IO ()
parseFile filename = withFile filename ReadMode parseContent
  where
    parseContent :: Handle -> IO ()
    parseContent h = do
      content <- hGetContents h
      putStrLn "Source code"
      putStrLn content
      putStrLn ""
      transform $ parserFile content

transform :: Either ParseError [(String, FctDef)] -> IO ()
transform (Left err) = print err
transform (Right code) = do -- TODO monad transformer
  putStrLn "AST"
  putStrLn $ concatMap formatCode code
  putStrLn ""
  let logicalInstrMap = exprToLogicalInstr $ fromList code
  either print transform2 logicalInstrMap
  where
    formatCode :: (String, FctDef) -> String
    formatCode (name, (nparams, ex)) = name ++ (concat [' ' : '*' : 'p' : show x | x <- [0 .. (nparams -1)]]) ++ " = " ++ show ex ++ "\n"

    transform2 :: Map String ([LogicalInstr], LogReg) -> IO ()
    transform2 logicalInstrMap = do
      putStrLn "Logical Instructions"
      putStrLn $ foldrWithKey (\k x acc -> acc ++ formatLogicalInstr k x ++ "\n") "" logicalInstrMap
      putStrLn ""
      let res = compile logicalInstrMap
      either print writeInstr res

      where
        formatLogicalInstr :: String -> ([LogicalInstr], LogReg) -> String
        formatLogicalInstr fname (instr, lreg) =
          let instructions = foldl' (\acc x -> acc ++ '\t' : show x ++ "\n") "" instr
            in fname ++ " :\n" ++ instructions ++ '\t' : "RET " ++ show lreg

compile :: Map String ([LogicalInstr], LogReg) -> Either CompileError [Instruction Reg8]
compile _ = Right [STOP $ Constant 0]

main :: IO ()
main = parseFile "test.asc"