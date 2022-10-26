module FonctionParser where

import CommonParser (MaybeParse, PartialParse, isWhitespace, readText)
import Context (FonctionParsingContext, ParamParsingContext)
import Data.Char (isAlpha)
import Data.Map as M (empty, insert, lookup, member, size)
import Expr (FonctionName (FonctionName))
import ParsingError (ParseError (..))
import RevString (add, toListRev, toTextRev)
import Zip (Parsed (..), createParse, merge, next)

type ParsedFonctionHeader = (FonctionName, ParamParsingContext, String)

type ParsedHeaderResult = (FonctionParsingContext, [ParsedFonctionHeader])

parseFonction :: FonctionParsingContext -> Parsed -> Either ParseError ParsedHeaderResult
parseFonction prog = parseFonctionInternal (prog, [])
  where
    parseFonctionInternal :: ParsedHeaderResult -> Parsed -> Either ParseError (FonctionParsingContext, [ParsedFonctionHeader])
    parseFonctionInternal result list@(CharParsed p@(old, x, xs))
      | isWhitespace x = parseFonctionInternal result (next p)
      | isAlpha x =
          let (suite, funcName) = readText list
           in let valide = when (funcName == "if") (Left InvalidFuncNameError)
               in valide >> parseFonctionHeader (FonctionName $ toText funcName) M.empty suite >>= integrateFuncInContext result
      | otherwise = Left $ UnrecognizedFuncChar (toTextRev old) x (toText xs)
    parseFonctionInternal result (EndParsed _) = pure result

    integrateFuncInContext :: ParsedHeaderResult -> PartialParse ParsedFonctionHeader -> Either ParseError ParsedHeaderResult
    integrateFuncInContext (context, fonctions) (suite, (funcName, params, code)) =
      let paramCount = size params
       in let valide = whenJust (lookup funcName context) (\x -> if x == paramCount then pass else Left InvalidParamCountNumber)
           in valide >> parseFonctionInternal (insert funcName paramCount context, (funcName, params, code) : fonctions) suite

    parseFonctionHeader :: FonctionName -> ParamParsingContext -> Parsed -> MaybeParse ParsedFonctionHeader
    parseFonctionHeader funcName params list@(CharParsed p@(old, x, xs))
      | x == '=' = (\(suite, body) -> (merge suite (add x old), (funcName, params, body))) <$> parseFonctionBody (createParse xs)
      | isWhitespace x = parseFonctionHeader funcName params $ next p
      | isAlpha x =
          let (suite, param) = readText list
           in let paramT = toText param
               in let valide = when (param == "if") (Left InvalidParamNameError) >> when (member paramT params) (Left InvalidParamNameError)
                   in valide >> parseFonctionHeader funcName (insert paramT (size params) params) suite
      | otherwise = Left $ UnrecognizedFuncChar (toTextRev old) x (toText xs)
    parseFonctionHeader _ _ (EndParsed txt) = Left $ NotAFunc $ toTextRev txt

    parseFonctionBody :: Parsed -> MaybeParse String
    parseFonctionBody (CharParsed p@(body, x, _))
      | x == '.' = pure (next p, toListRev body)
      | otherwise = parseFonctionBody $ next p
    parseFonctionBody (EndParsed txt) = pure (EndParsed txt, toListRev txt)