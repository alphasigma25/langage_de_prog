{-# LANGUAGE InstanceSigs #-}

module RevString (RevString, addRS, isWhiteSpace) where

import Data.Char (isSpace)
import Data.Foldable (Foldable (foldl'))

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

isWhiteSpace :: RevString -> Bool
isWhiteSpace (RevString rev) = foldl' (\acc c -> acc && isSpace c) True rev

addRS :: Char -> RevString -> RevString
addRS c (RevString str) = RevString (c : str)