module Parser.Helper where

import Text.Read (readMaybe)

readMaybeInt :: String -> Maybe Int
readMaybeInt a = readMaybe a >>= applyFilter

applyFilter :: Integer -> Maybe Int
applyFilter a = if filterIntInteger a then Just $ fromInteger a else Nothing

filterIntInteger :: Integer -> Bool
filterIntInteger a = (a < toInteger (maxBound :: Int)) && (a > toInteger (minBound :: Int))

maybeToEither :: Maybe a -> b -> Either b a
maybeToEither a b = maybe (Left b) Right a