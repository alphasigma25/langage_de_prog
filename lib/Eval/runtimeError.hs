module RuntimeError where

data RuntimeError = DivByZero | InvalidInstruction | InvalidParamIndex | MissingFunction | InvalidParamCount deriving (Show)