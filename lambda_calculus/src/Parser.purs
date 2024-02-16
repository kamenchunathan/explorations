module Parser
  ( LTerm(..)
  , lambdaCalculusParser
  )
  where

import Prelude hiding (when, between)

import Control.Alternative ((<|>))
import Control.Lazy (defer)
import Data.Foldable (fold)
import Data.List (List)
import Data.List.Lazy (replicate)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.String.CodePoints (singleton)
import Parsing (Parser, Position(..), fail, position)
import Parsing.Combinators (between, optionMaybe)
import Parsing.Token (match, when)
import Tokenizer (Token(..), isVariableToken)

--| An term in the lambda calculus
data LTerm
  --| A single variable is a valid lambda term
  = LVar CodePoint
  --| Abstraction of two lambda terms
  | LAbs CodePoint LTerm
  --| Application of two lambda terms
  | LApp LTerm LTerm

instance Show LTerm where
  show t = "\n" <> (innerShow 1 t) <> "\n"

instance Eq LTerm where
  eq (LVar c1) (LVar c2) = c1 == c2
  eq (LAbs c1 t1) (LAbs c2 t2) = c1 == c2 && t1 == t2
  eq (LApp t1 t2) (LApp t3 t4) = t1 == t3 && t2 == t4
  eq _ _ = false

innerShow :: Int -> LTerm -> String
innerShow identCount term =
  let
    idents = (fold $ replicate identCount "  ")
  in
    case term of
      (LVar c) -> "(Var:" <> singleton c <> ")"
      (LAbs x t) -> "(Abs:\n"
        <> idents
        <> singleton x
        <> "\n"
        <> idents
        <> (innerShow (identCount + 1) t)
        <> ")"
      (LApp f x) -> "(App:\n"
        <> idents
        <> innerShow (identCount + 1) f
        <> "\n"
        <> idents
        <> innerShow (identCount + 1) x
        <> ")"

lambdaCalculusParser :: Parser (List Token) LTerm
lambdaCalculusParser = termParser

termParser :: Parser (List Token) LTerm
termParser = do
  t1 <-
    ( defer \_ ->
        variableParser
          <|> (defer \_ -> parenthesizedTermParser)
          <|> (defer \_ -> abstractionParser)

    )
  -- if there is a next term then this is an application else just a term
  optNext <- optionMaybe termParser
  case optNext of
    Just t2 -> pure $ LApp t1 t2
    Nothing -> pure t1

parenthesizedTermParser :: Parser (List Token) LTerm
parenthesizedTermParser =
  between (tokenParser LParen) (tokenParser RParen) (defer \_ -> termParser)

abstractionParser :: Parser (List Token) LTerm
abstractionParser = do
  (Position pos) <- position
  _ <- match (\_ -> Position $ pos { index = pos.index + 1 }) BackSlash
  boundVar <- boundVariableParser
  _ <- tokenParser Period
  innerTerm <- (defer \_ -> termParser)
  pure $ LAbs boundVar innerTerm

variableParser :: Parser (List Token) LTerm
variableParser = do
  (Position pos) <- position
  varToken <- when (\_ -> Position $ pos { index = pos.index + 2 }) isVariableToken
  case varToken of
    Variable c -> pure $ LVar c
    _ -> fail "Variable parser: Should never fail due to check beforehand"

boundVariableParser :: Parser (List Token) CodePoint
boundVariableParser = do
  (Position pos) <- position
  varToken <- when (\_ -> Position $ pos { index = pos.index + 2 }) isVariableToken
  case varToken of
    Variable c -> pure c
    _ -> fail "Variable parser: Should never fail due to check beforehand"

tokenParser :: Token -> Parser (List Token) Token
tokenParser t = do
  (Position pos) <- position
  match (\_ -> Position $ pos { index = pos.index + 1 }) t

