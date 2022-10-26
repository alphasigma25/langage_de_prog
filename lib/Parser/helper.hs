module Parser.Helper where

import Text.Read (readMaybe)

readMaybeInt :: String -> Maybe Int
readMaybeInt a = readMaybe a >>= applyFilter
  where
    applyFilter :: Integer -> Maybe Int
    applyFilter b = if filterIntInteger b then Just $ fromInteger b else Nothing
      where
        filterIntInteger :: Integer -> Bool
        filterIntInteger c = (c < toInteger (maxBound :: Int)) && (c > toInteger (minBound :: Int))