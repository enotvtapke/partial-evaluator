module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)
import qualified Flowchart.InterpreterSpec as F (interpreterSpec)
import qualified Turing.InterpreterSpec as T (interpreterSpec) 
import Flowchart.MixSpec (mixSpec)

main :: IO ()
main = do
  specs <-
    concat
      <$> mapM
        testSpecs
        [ F.interpreterSpec,
          T.interpreterSpec,
          mixSpec
        ]
  defaultMain
    ( testGroup
        "Main Tests"
        [ testGroup "Specs" specs
        ]
    )
