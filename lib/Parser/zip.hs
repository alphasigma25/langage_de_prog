{-# LANGUAGE InstanceSigs #-}

module Parser.Zip where

import Parser.RevString (RevString, add)
import qualified Text.Show


data Parsed = CharParsed (RevString, Char, String) | EndParsed RevString

instance Show Parsed where
  show :: Parsed -> String
  show p = show (parsedPart p) ++ '*' : futurePart p

parsedPart :: Parsed -> RevString
parsedPart (CharParsed (old, _, _)) = old
parsedPart (EndParsed old) = old

futurePart :: Parsed -> String
futurePart (CharParsed (_, x, xs)) = x : xs
futurePart (EndParsed _) = []

incomplete :: Parsed -> Bool
incomplete CharParsed {} = True
incomplete (EndParsed _) = False

createParse :: String -> Parsed
createParse (x : xs) = CharParsed (mempty, x, xs)
createParse [] = EndParsed mempty

merge :: Parsed -> RevString -> Parsed
merge (CharParsed (old, x, xs)) old1 = CharParsed (old1 <> old, x, xs)
merge (EndParsed old) old1 = EndParsed $ old1 <> old

next :: (RevString, Char, [Char]) -> Parsed
next (old, oldX, x : xs) = CharParsed (add oldX old, x, xs)
next (old, x, []) = EndParsed $ add x old