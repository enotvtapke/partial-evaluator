module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)
import InterpreterSpec (interpreterSpec)

main :: IO ()
main = do
  specs <-
    concat
      <$> mapM
        testSpecs
        [ interpreterSpec
        ]
  defaultMain
    ( testGroup
        "Main Tests"
        [ testGroup "Specs" specs
        ]
    )
