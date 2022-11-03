module Helper where

import Text.Read (readMaybe)

readMaybeInt :: String -> Either Bool Int
readMaybeInt a = maybe (Left False) Right (readMaybe a) >>= applyFilter
  where
    applyFilter :: Integer -> Either Bool Int
    applyFilter b = if filterIntInteger then Right $ fromInteger b else Left True
      where
        filterIntInteger :: Bool
        filterIntInteger = (b < toInteger (maxBound :: Int)) && (b > toInteger (minBound :: Int))