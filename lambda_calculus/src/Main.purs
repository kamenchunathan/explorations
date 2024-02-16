module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Parser (lambdaCalculusParser)
import Parsing (runParser)
import Tokenizer (tokenize)

main :: Effect Unit
main = do
  let tokens = tokenize "x(\\x.xx)y"
  logShow tokens
  logShow $ runParser tokens lambdaCalculusParser

