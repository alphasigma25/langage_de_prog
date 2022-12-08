{-# LANGUAGE BinaryLiterals #-}

module Main where

import CompileError (CompileError)
import Control.Monad ((>=>))
import Data.ByteString.Builder (hPutBuilder)
import Data.Map (Map, fromList)
import Expr (FctDef)
import ExprToLogical (LogicalInstr, exprToLogicalInstr, LogReg)
import Parser (ParseError, parserFile)
import RegToBinary (Instruction (..), Reg, Src1 (Constant), programToBin)
import Register8 (Reg8)
import System.IO (Handle, IOMode (ReadMode, WriteMode), hGetContents, withFile)

writeInstr :: Reg regType => [Instruction regType] -> IO ()
writeInstr prog = withFile "result.asc" WriteMode (`hPutBuilder` programToBin prog)

parseFile :: String -> IO ()
parseFile filename = withFile filename ReadMode (parseContent >=> transform)
  where
    parseContent :: Handle -> IO (Either ParseError [(String, FctDef)])
    parseContent h = parserFile <$> hGetContents h

transform :: Either ParseError [(String, FctDef)] -> IO ()
transform (Left err) = print err
transform (Right code) = either print writeInstr $ compile code

compile :: [(String, FctDef)] -> Either CompileError [Instruction Reg8]
compile = exprToLogicalInstr . fromList >=> compile2

compile2 :: Map String ([LogicalInstr], LogReg) -> Either CompileError [Instruction Reg8]
compile2 _ = Right [STOP $ Constant 0]

main :: IO ()
main = parseFile "test.asc"