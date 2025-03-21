{-# LANGUAGE OverloadedStrings #-}

module Flowchart.MixSpec (mixSpec) where

import Flowchart.AST
import Flowchart.DSL
import Flowchart.DivisionCalculator (programStaticVars, programStaticVars')
import Flowchart.Mix
import Flowchart.TestPrograms
import Test.Hspec
import TestUtils
import Turing.Interpreter (turingInterpreter)
import Turing.TestPrograms (replaceFirstOne)
import Prelude hiding ((+), (==))

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
      programStaticVars' mix [VarName "program", VarName "staticVars"] `shouldBe` VarName <$> ["bb", "command", "cond", "exp", "liveVars", "pp'", "ppFalse", "ppTrue", "pps", "program", "staticVars", "varExpr", "varName"]

spec_search :: Spec
spec_search =
  describe "search" $ do
    it "interpretes search" $
      (searchProgram, [s "a", list [s "b", s "a", s "c"], list [s "bv", s "av", s "cv"]]) `interShouldBe` s "av"
    it "mixes search" $
      (mix, [prog searchProgram, programStaticVars searchProgram ["name", "namelist"], list [pair (s "name") (s "c"), pair (s "namelist") $ list [s "b", s "a", s "c"]]]) `interShouldBe` prog mixedSearchProgram

spec_turing_machine :: Spec
spec_turing_machine =
  describe "turing machine mix" $ do
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
    it "run second proj generated compiler on `replaceFirstOne`" $
      (runProgram mix [prog mix, programStaticVars mix ["program", "staticVars"], list [pair (s "program") (prog turingInterpreter), pair (s "staticVars") (programStaticVars turingInterpreter ["q"])]], [list [pair (s "q") replaceFirstOne]]) `interShouldBe` prog mixedTuringProgram
    it "second proj" $
      (mix, [prog mix, programStaticVars mix ["program", "staticVars"], list [pair (s "program") (prog turingInterpreter), pair (s "staticVars") (programStaticVars turingInterpreter ["q"])]]) `interShouldBe` prog generatedTuringMachineCompilerProgram
    xit "third proj" $
      (mix, [prog mix, programStaticVars mix ["program", "staticVars"], list [pair (s "program") (prog mix), pair (s "staticVars") (programStaticVars mix ["program", "staticVars"])]]) `interShouldBe` unit
    it "run third proj to generate turing machine compiler" $
      ( runProgram
          mix
          [ prog mix,
            programStaticVars mix ["program", "staticVars"],
            list
              [ pair (s "program") (prog mix),
                pair (s "staticVars") (programStaticVars mix ["program", "staticVars"])
              ]
          ],
        [ list
            [ pair (s "program") (prog turingInterpreter),
              pair (s "staticVars") (programStaticVars turingInterpreter ["q"])
            ]
        ]
      )
        `interShouldBe` prog generatedTuringMachineCompilerProgram
    it "run third proj generated compiler on `replaceFirstOne`" $
      ( runProgram
          compilerGeneratorProgram
          [ list
              [ pair (s "program") (prog turingInterpreter),
                pair (s "staticVars") (programStaticVars turingInterpreter ["q"])
              ]
          ],
        [list [pair (s "q") replaceFirstOne]]
      )
        `interShouldBe` prog mixedTuringProgram
