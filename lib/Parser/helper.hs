module Parser.Helper where

import Text.Read (readMaybe)

readMaybeInt :: String -> Either Bool Int
readMaybeInt a = maybe (Left False) Right (readMaybe a) >>= applyFilter
  where
    applyFilter :: Integer -> Either Bool Int
    applyFilter b = if filterIntInteger b then Right $ fromInteger b else Left True
      where
        filterIntInteger :: Integer -> Bool
        filterIntInteger c = (c < toInteger (maxBound :: Int)) && (c > toInteger (minBound :: Int))