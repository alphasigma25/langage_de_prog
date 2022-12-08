{-# LANGUAGE InstanceSigs #-}

module Register4 (Reg4, getRegNum) where

import Data.Int (Int16)
import RegToBinary (Reg (..))

data Reg4 = Reg0 | Reg1 | Reg2 | Reg3

instance Reg Reg4 where
  getRegNum :: Reg4 -> Int16
  getRegNum Reg0 = 0
  getRegNum Reg1 = 1
  getRegNum Reg2 = 2
  getRegNum Reg3 = 3