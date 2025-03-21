module Flowchart.LiveVariableAnalyserSpec (liveVariableAnalyserSpec) where

import Data.Bifunctor (Bifunctor (..), bimap)
import qualified Data.HashMap.Lazy as M
import Flowchart.AST
import Flowchart.LiveVariablesAnalyser (liveVariables)
import Flowchart.TestPrograms (searchProgram)
import Test.Hspec

liveVariableAnalyserSpec :: Spec
liveVariableAnalyserSpec = describe "Live Variable Analyser" $ do
  spec_basic

spec_basic :: Spec
spec_basic =
  describe "basic" $ do
    it "analyses searchProgram" $
      liveVariables searchProgram `shouldBe` M.fromList (map (bimap Label (map VarName)) [("found", ["valuelist"]), ("init", ["name", "namelist", "valuelist"]), ("search", ["name", "namelist", "valuelist"]), ("cont", ["name", "namelist", "valuelist"])])
