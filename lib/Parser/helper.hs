module Parser.Helper where

import Text.Read (readMaybe)

readMaybeInt :: String -> Either Bool Int
readMaybeInt a = maybe (Left True) Right (readMaybe a) >>= applyFilter

applyFilter :: Integer -> Either Bool Int
applyFilter a = if filterIntInteger a then Right $ fromInteger a else Left False

filterIntInteger :: Integer -> Bool
filterIntInteger a = (a < toInteger (maxBound :: Int)) && (a > toInteger (minBound :: Int))