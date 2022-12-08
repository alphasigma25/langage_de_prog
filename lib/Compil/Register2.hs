{-# LANGUAGE InstanceSigs #-}

module Register2 (Reg2, getRegNum) where

import Data.Int (Int16)
import RegToBinary (Reg (..))

data Reg2 = Reg0 | Reg1

instance Reg Reg2 where
  getRegNum :: Reg2 -> Int16
  getRegNum Reg0 = 0
  getRegNum Reg1 = 1