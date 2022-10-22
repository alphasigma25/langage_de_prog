{-# LANGUAGE InstanceSigs #-}

module Parser.RevString where

import qualified Text.Show

newtype RevString = RevString String

instance Show RevString where
  show :: RevString -> String
  show (RevString s) = reverse s

instance Semigroup RevString where
  (<>) :: RevString -> RevString -> RevString
  (<>) (RevString s1) (RevString s2) = RevString $ s2 ++ s1

instance Monoid RevString where
  mempty :: RevString
  mempty = RevString ""

add :: Char -> RevString -> RevString
add car (RevString s) = RevString $ car : s