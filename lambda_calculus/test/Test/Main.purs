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
  assertEqual
    { actual: runParser (tokenize "xx") lambdaCalculusParser
    , expected: (Right (LApp (LVar $ codePointFromChar 'x') (LVar $ codePointFromChar 'x')))
    }
  assertEqual
    { actual: runParser (tokenize "xyz") lambdaCalculusParser
    , expected:
        ( Right
            ( LApp
                ( LApp
                    (LVar $ codePointFromChar 'x')
                    (LVar $ codePointFromChar 'y')
                )
                (LVar $ codePointFromChar 'z')
            )
        )
    }

