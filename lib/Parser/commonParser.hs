module CommonParser where

import Data.Char (isAlphaNum)
import ParsingError (ParseError)
import RevString (RevString, add, toListRev)
import Zip (Parsed (CharParsed), next)

type PartialParse x = (Parsed, x)

type MaybeParse x = Either ParseError (PartialParse x)

readText :: Parsed -> PartialParse String
readText = readTextInternal mempty
  where
    readTextInternal :: RevString -> Parsed -> PartialParse String
    readTextInternal txt (CharParsed p@(_, x, _))
      | isAlphaNum x = readTextInternal (add x txt) $ next p
    readTextInternal txt list = (list, toListRev txt)

isWhitespace :: Char -> Bool
isWhitespace x = x == '\n' || x == ' '