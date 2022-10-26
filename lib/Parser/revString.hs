{-# LANGUAGE InstanceSigs #-}

module RevString (RevList, toListRev, toTextRev, add, RevString) where

newtype RevList x = RevList [x]

type RevString = RevList Char

instance Semigroup (RevList x) where
  (<>) :: RevList x -> RevList x -> RevList x
  (<>) (RevList s1) (RevList s2) = RevList $ s2 ++ s1

instance Monoid (RevList x) where
  mempty :: RevList x
  mempty = RevList mempty

toListRev :: RevList a -> [a]
toListRev (RevList s) = reverse s

toTextRev :: RevList Char -> Text
toTextRev (RevList s) = toText $ reverse s

add :: x -> RevList x -> RevList x
add car (RevList s) = RevList $ car : s