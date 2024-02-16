module Tokenizer
  ( Token(..)
  , tokenize
  , isVariableToken
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.CodePoint.Unicode (isAscii, isSpace)
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, codePointFromChar, singleton, toCodePointArray)

data Token
  = BackSlash
  | Period
  | Variable CodePoint
  | RParen
  | LParen

instance Show Token where
  show = case _ of
    BackSlash -> "Token.BackSlash"
    Period -> "Token.Period"
    LParen -> "Token.LParen"
    RParen -> "Token.RParen"
    Variable c -> "Token.Variable ( " <> singleton c <> " )"

instance Eq Token where
  eq BackSlash BackSlash = true
  eq Period Period = true
  eq LParen LParen = true
  eq RParen RParen = true
  eq (Variable a) (Variable b) = a == b
  eq _ _ = false

tokenize :: String -> List Token
tokenize input = fromFoldable $ catMaybes $ map toToken $ toCodePointArray input

toToken :: CodePoint -> Maybe Token
toToken c
  | c == codePointFromChar '\\' = Just BackSlash
  | c == codePointFromChar '.' = Just Period
  | c == codePointFromChar '(' = Just LParen
  | c == codePointFromChar ')' = Just RParen
  | isAscii c && not (isSpace c) = Just (Variable c)
  | otherwise = Nothing

isVariableToken :: Token -> Boolean
isVariableToken =
  case _ of
    Variable _ -> true
    _ -> false
