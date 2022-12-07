{-# LANGUAGE BinaryLiterals #-}

module Main where

import Compil (Instruction (..), programToBin)
import Data.ByteString.Builder (hPutBuilder)
import Expr (FctDef)
import Parser (ParseError, parserFile)
import System.IO (Handle, IOMode (ReadMode, WriteMode), hGetContents, withFile)

writeInstr :: [Instruction] -> IO ()
writeInstr prog = withFile "result.asc" WriteMode (`hPutBuilder` programToBin prog)

parseFile :: String -> IO (Either ParseError [(String, FctDef)])
parseFile filename = withFile filename ReadMode parseContent
  where
    parseContent :: Handle -> IO (Either ParseError [(String, FctDef)])
    parseContent h = parserFile <$> hGetContents h

transform :: Either ParseError [(String, FctDef)] -> IO ()
transform (Left err) = print err
transform (Right code) = either print writeInstr $ compile code

compile :: [(String, FctDef)] -> Either ParseError [Instruction]
compile = undefined

main :: IO ()
main = parseFile "test.asc" >>= transform