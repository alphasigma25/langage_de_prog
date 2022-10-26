module Context where

import Expr (FonctionName, Program, FonctionArgCount (FonctionArgCount))

type ParamParsingContext = Map Text Int

type FonctionParsingContext = Map FonctionName Int

parsingContextFromProg :: Program -> FonctionParsingContext
parsingContextFromProg prog = (\(_, FonctionArgCount val) -> val) <$> prog