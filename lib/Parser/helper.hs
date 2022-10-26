module Helper where

readMaybeInt :: String -> Either Bool Int
readMaybeInt a = maybeToRight True (readMaybe a) >>= applyFilter
  where
    applyFilter :: Integer -> Either Bool Int
    applyFilter filt =
      let valid = (filt < toInteger (maxBound :: Int)) && (filt > toInteger (minBound :: Int))
       in if valid then pure $ fromInteger filt else Left False