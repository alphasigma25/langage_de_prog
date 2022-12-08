{-# LANGUAGE InstanceSigs #-}

module Register8 (Reg8, getRegNum) where

import Data.Int (Int16)
import RegToBinary (Reg (..))

data Reg8 = Reg0 | Reg1 | Reg2 | Reg3 | Reg4 | Reg5 | Reg6 | Reg7

instance Reg Reg8 where
  getRegNum :: Reg8 -> Int16
  getRegNum Reg0 = 0
  getRegNum Reg1 = 1
  getRegNum Reg2 = 2
  getRegNum Reg3 = 3
  getRegNum Reg4 = 4
  getRegNum Reg5 = 5
  getRegNum Reg6 = 6
  getRegNum Reg7 = 7