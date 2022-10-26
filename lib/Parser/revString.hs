{-# LANGUAGE InstanceSigs #-}

module Parser.RevString where

newtype RevString = RevString String

instance Show RevString where
  show :: RevString -> String
  show (RevString str) = reverse str

instance Semigroup RevString where
  (<>) (RevString rs1) (RevString rs2) = RevString $ rs2 ++ rs1

instance Monoid RevString where
  mempty = RevString ""

addRS :: Char -> RevString -> RevString
addRS c (RevString str) = RevString (c : str)