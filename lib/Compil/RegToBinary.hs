{-# LANGUAGE BinaryLiterals #-}

module RegToBinary (Reg (getRegNum), programToBin, Instruction (..), Src1 (..)) where

import Data.Bits (shiftL)
import Data.ByteString.Builder (Builder, int16LE)
import Data.Int (Int16)
import Expr (Op (..))

getOpNum :: Op -> Int16
getOpNum a = shiftL (getOpNumInternal a) 10
  where
    getOpNumInternal :: Op -> Int16
    getOpNumInternal Add = 0
    getOpNumInternal Sub = 1
    getOpNumInternal Mul = 2
    getOpNumInternal Div = 3
    getOpNumInternal Mod = 4
    getOpNumInternal RSub = 5
    getOpNumInternal RMod = 6
    getOpNumInternal RDiv = 7

class Reg reg where
  getRegNum :: reg -> Int16

data Src1 regType = Registre regType | Constant Int16

data Instruction regType
  = STOP (Src1 regType)
  | CONST (Src1 regType) regType
  | OP Op (Src1 regType) regType regType

immBit :: Int16
immBit = shiftL 1 9

genOpCode :: Int16 -> Int16
genOpCode code = shiftL code 13

genR1 :: Reg regType => regType -> Int16
genR1 r = shiftL (getRegNum r) 0

genR2 :: Reg regType => regType -> Int16
genR2 r = shiftL (getRegNum r) 3

genRd :: Reg regType => regType -> Int16
genRd r = shiftL (getRegNum r) 6

convertToBin :: Reg regType => Instruction regType -> Either Int16 (Int16, Int16)
convertToBin ins = (\(x, y) -> (x + immBit, y)) <$> convertToBinInternal ins
  where
    convertToBinInternal :: Reg regType => Instruction regType -> Either Int16 (Int16, Int16)
    convertToBinInternal (STOP (Registre r)) = Left $ genOpCode 0 + getRegNum r
    convertToBinInternal (STOP (Constant c)) = Right (genOpCode 0, c)
    convertToBinInternal (CONST (Registre r) dest) = Left $ genOpCode 1 + genRd dest + genR1 r
    convertToBinInternal (CONST (Constant c) dest) = Right (genOpCode 1 + genRd dest, c)
    convertToBinInternal (OP op (Registre r1) r2 dest) = Left $ getOpNum op + genOpCode 2 + genRd dest + genR2 r2 + genR1 r1
    convertToBinInternal (OP op (Constant c) r dest) = Right (getOpNum op + genOpCode 2 + genRd dest + genR2 r, c)

programToBin :: Reg regType => [Instruction regType] -> Builder -- builder = monoïd
programToBin instr =
  mconcat $ fmap (either int16LE (\(x1, x2) -> int16LE x1 <> int16LE x2) . convertToBin) instr