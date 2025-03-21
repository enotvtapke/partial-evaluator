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
import Flowchart.DivisionCalculator (programStaticVars, programStaticVars')

mixSpec :: Spec
mixSpec = describe "Mix" $ do
  spec_divisionCalculator
  spec_search
  spec_descrToProg
  spec_turing_machine
  spec_secondProjection

spec_divisionCalculator :: Spec
spec_divisionCalculator =
  describe "programStaticVars" $ do
    it "programStaticVars of `searchProgram`" $
      programStaticVars' searchProgram [VarName "name", VarName "namelist"] `shouldBe` [VarName "name", VarName "namelist"]
    it "programStaticVars of `mix`" $
      programStaticVars' mix [VarName "program", VarName "staticVars"] `shouldBe` VarName <$> ["bb", "command", "cond", "exp", "pp'", "ppFalse", "ppTrue", "pps", "program", "staticVars", "varExpr", "varName"]

spec_search :: Spec
spec_search =
  describe "search" $ do
    it "interpretes search" $
      (searchProgram, [s "a", list [s "b", s "a", s "c"], list [s "bv", s "av", s "cv"]]) `interShouldBe` s "av"
    it "mixes search" $
      (mix, [prog searchProgram, programStaticVars searchProgram ["name", "namelist"], list [pair (s "name") (s "c"), pair (s "namelist") $ list [s "b", s "a", s "c"]]]) `interShouldBe` prog mixedSearchProgram

spec_turing_machine :: Spec
spec_turing_machine =  describe "turing machine mix" $ do
    it "mixes turing machine interpreter" $
      (mix, [prog turingInterpreter, programStaticVars turingInterpreter ["q"], list [pair (s "q") replaceFirstOne]]) `interShouldBe` prog mixedTuringProgram

spec_descrToProg :: Spec
spec_descrToProg =
  describe "descrToProg" $ do
    it "interpretes descrToProg" $
      (descrToProgProgram, [prog searchProgram, list [pair (s "name") (s "a")]]) `interShouldBe` prog descrProgram

spec_secondProjection :: Spec
spec_secondProjection =
  describe "secondProjection" $ do
    it "run second proj" $
      (runMixed mix [prog mix, programStaticVars mix ["program", "staticVars"], list [pair (s "program") (prog turingInterpreter), pair (s "staticVars") (programStaticVars turingInterpreter ["q"])]], [list [pair (s "q") replaceFirstOne]]) `interShouldBe` prog mixedTuringProgram
    xit "second proj" $
      (mix, [prog mix, programStaticVars mix ["program", "staticVars"], list [pair (s "program") (prog turingInterpreter), pair (s "staticVars") (programStaticVars turingInterpreter ["q"])]]) `interShouldBe` int 2
    xit "third proj" $
      (mix, [prog mix, programStaticVars mix ["program", "staticVars"], list [pair (s "program") (prog mix), pair (s "staticVars") (programStaticVars mix ["program", "staticVars"])]]) `interShouldBe` int 2
    xit "run third proj" $
      (runMixed mix [prog mix, programStaticVars mix ["program", "staticVars"], list [pair (s "program") (prog mix), pair (s "staticVars") (programStaticVars mix ["program", "staticVars"])]], [list [pair (s "program") (prog turingInterpreter)]]) `interShouldBe` int 2

runMixed :: Program -> [Expr] -> Program
runMixed p args = case runEvalMonad (interpret p args) of
  Right (Prog p) -> p
