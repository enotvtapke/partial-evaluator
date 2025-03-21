module Flowchart.LiveVariableAnalyserSpec (liveVariableAnalyserSpec) where

import Data.Bifunctor (bimap)
import qualified Data.HashMap.Lazy as M
import Flowchart.AST
import Flowchart.LiveVariablesAnalyser (liveVariables)
import Flowchart.Mix (mix)
import Test.Hspec

liveVariableAnalyserSpec :: Spec
liveVariableAnalyserSpec = describe "Live Variable Analyser" $ do
  spec_basic

spec_basic :: Spec
spec_basic =
  describe "basic" $ do
    it "analyses mix" $
      liveVariables mix
        `shouldBe` M.fromList
          ( map
              (bimap Label (map VarName))
              [("a", ["b", "k"]), ("c", ["d"])]
          )
    it "analyses 2 mix" $
      M.map (\v -> length v) (liveVariables mix)
        `shouldBe` M.fromList
          ( map
              (bimap Label (id))
              [("a", 2), ("c",3)]
          )
