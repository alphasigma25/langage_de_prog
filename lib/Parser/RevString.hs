{-# LANGUAGE InstanceSigs #-}

module RevString (RevString, addRS) where

newtype RevString = RevString String

instance Show RevString where
  show :: RevString -> String
  show (RevString str) = reverse str

instance Semigroup RevString where
  (<>) :: RevString -> RevString -> RevString
  (<>) (RevString rs1) (RevString rs2) = RevString $ rs2 ++ rs1

instance Monoid RevString where
  mempty :: RevString
  mempty = RevString ""

addRS :: Char -> RevString -> RevString
addRS c (RevString str) = RevString (c : str)