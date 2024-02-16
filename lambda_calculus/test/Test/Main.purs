module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.String (codePointFromChar)
import Effect (Effect)
import Parser (LTerm(..), lambdaCalculusParser)
import Parsing (runParser)
import Test.Assert (assertEqual)
import Tokenizer (tokenize)

main :: Effect Unit
main = do
  let res = runParser (tokenize "xx") lambdaCalculusParser
  assertEqual
    { actual: res
    , expected: (Right (LApp (LVar $ codePointFromChar 'x') (LVar $ codePointFromChar 'x')))
    }

