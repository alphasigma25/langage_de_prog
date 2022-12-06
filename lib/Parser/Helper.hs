module Helper where

import Data.Int (Int16)
import Text.Read (readMaybe)

readMaybeInt :: String -> Either Bool Int16
readMaybeInt a = maybe (Left False) Right (readMaybe a) >>= applyFilter
  where
    applyFilter :: Integer -> Either Bool Int16
    applyFilter b = if filterIntInteger then Right $ fromInteger b else Left True
      where
        filterIntInteger :: Bool
        filterIntInteger = (b < toInteger (maxBound :: Int16)) && (b > toInteger (minBound :: Int16))