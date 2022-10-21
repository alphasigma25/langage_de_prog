{-# LANGUAGE InstanceSigs #-}

module Parser.RevString where

newtype RevString = RevString String

instance Show RevString where
  show :: RevString -> String
  show (RevString s) = reverse s

instance Semigroup RevString where
  (<>) :: RevString -> RevString -> RevString
  (<>) (RevString first) (RevString second) = RevString $ second ++ first

instance Monoid RevString where
  mempty :: RevString
  mempty = RevString ""

add :: Char -> RevString -> RevString
add car (RevString s) = RevString $ car : s