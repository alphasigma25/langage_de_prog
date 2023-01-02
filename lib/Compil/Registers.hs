{-# LANGUAGE InstanceSigs #-}

module Registers (Reg, Reg2, Reg4, Reg8, getRegNum) where

import Data.Int (Int16)

class Reg reg where
  getRegNum :: reg -> Int16

data Reg2 = Reg2_0 | Reg2_1

instance Reg Reg2 where
  getRegNum :: Reg2 -> Int16
  getRegNum Reg2_0 = 0
  getRegNum Reg2_1 = 1

data Reg4 = Reg4_0 | Reg4_1 | Reg4_2 | Reg4_3

instance Reg Reg4 where
  getRegNum :: Reg4 -> Int16
  getRegNum Reg4_0 = 0
  getRegNum Reg4_1 = 1
  getRegNum Reg4_2 = 2
  getRegNum Reg4_3 = 3

data Reg8 = Reg8_0 | Reg8_1 | Reg8_2 | Reg8_3 | Reg8_4 | Reg8_5 | Reg8_6 | Reg8_7

instance Reg Reg8 where
  getRegNum :: Reg8 -> Int16
  getRegNum Reg8_0 = 0
  getRegNum Reg8_1 = 1
  getRegNum Reg8_2 = 2
  getRegNum Reg8_3 = 3
  getRegNum Reg8_4 = 4
  getRegNum Reg8_5 = 5
  getRegNum Reg8_6 = 6
  getRegNum Reg8_7 = 7