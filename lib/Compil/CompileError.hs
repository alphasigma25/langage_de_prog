{-# LANGUAGE InstanceSigs #-}

module CompileError where

data CompileError
    = UndefError

instance Show CompileError where
    show :: CompileError -> String
    show UndefError = "Compile Error : undefined error"