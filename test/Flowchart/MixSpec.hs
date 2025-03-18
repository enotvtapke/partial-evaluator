{-# LANGUAGE OverloadedStrings #-}

module Flowchart.MixSpec (mixSpec) where

import Flowchart.DSL
import Flowchart.Mix
import Test.Hspec
import TestUtils
import Prelude hiding ((+), (==))
import Flowchart.TestPrograms
import Turing.Interpreter (turingInterpreter)
import Turing.TestPrograms (replaceFirstOne)
import Flowchart.Interpreter.Interpreter (interpret)
import Flowchart.AST
import Flowchart.Interpreter.EvalState (runEvalMonad)

mixSpec :: Spec
mixSpec = describe "Mix" $ do
  spec_search
  spec_descrToProg
  spec_turing_machine
  spec_secondProjection

spec_search :: Spec
spec_search =
  describe "search" $ do
    it "mixes search" $
      (mix, [prog searchProgram, list [pair (s "name") (s "c"), pair (s "namelist") $ list [s "b", s "a", s "c"]]]) `interShouldBe` prog mixedSearchProgram
    it "interpretes search" $
      (searchProgram, [s "a", list [s "b", s "a", s "c"], list [s "bv", s "av", s "cv"]]) `interShouldBe` s "av"

spec_turing_machine :: Spec
spec_turing_machine =  describe "turing machine mix" $ do
    it "mixes turing machine interpreter" $
      (mix, [prog turingInterpreter, list [pair (s "q") replaceFirstOne]]) `interShouldBe` prog mixedTuringProgram

spec_descrToProg :: Spec
spec_descrToProg =
  describe "descrToProg" $ do
    it "interpretes descrToProg" $
      (descrToProgProgram, [prog searchProgram, list [pair (s "name") (s "a")]]) `interShouldBe` prog descrProgram

spec_secondProjection :: Spec
spec_secondProjection =
  describe "secondProjection" $ do
    it "second proj" $
      (mix, [prog mix, list [pair (s "program") (prog turingInterpreter)]]) `interShouldBe` int 2
    -- it "run second proj" $
    --   (runMixed mix [prog mix, list [pair (s "program") (prog turingInterpreter)]], [list [pair (s "q") replaceFirstOne]]) `interShouldBe` int 2
    -- it "third proj" $
    --   (mix, [prog mix, list [pair (s "program") (prog mix)]]) `interShouldBe` int 2
    -- it "run third proj" $
    --   (runMixed mix [prog mix, list [pair (s "program") (prog mix)]], [list [pair (s "program") (prog turingInterpreter)]]) `interShouldBe` int 2
    
runMixed :: Program -> [Expr] -> Program
runMixed p args = case runEvalMonad (interpret p args) of
  Right (Prog p) -> p
