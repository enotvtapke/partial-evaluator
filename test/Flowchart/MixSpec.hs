{-# LANGUAGE OverloadedStrings #-}

module Flowchart.MixSpec (mixSpec) where

import Flowchart.AST
import Flowchart.DSL
import Flowchart.Mix
import Test.Hspec
import TestUtils
import Prelude hiding ((+), (==))
import Flowchart.Interpreter.EvalState (runEvalMonad)
import Flowchart.Interpreter.Interpreter (evalExpr)

mixSpec :: Spec
mixSpec = describe "Flowchart Mix" $ do
  spec_search
  spec_descrToProg

spec_search :: Spec
spec_search =
  describe "search" $ do
    it "mixes search" $
      (mix, [prog searchProgram, list [pair (s "name") (s "c"), pair (s "namelist") $ list [s "b", s "a", s "c"]]]) `interShouldBe` prog mixedSearch
    it "interpretes search" $
      (searchProgram, [s "a", list [s "b", s "a", s "c"], list [s "bv", s "av", s "cv"]]) `interShouldBe` s "av"

spec_descrToProg :: Spec
spec_descrToProg =
  describe "descrToProg" $ do
    it "interpretes descrToProg" $
      (descrToProgProgram, [prog searchProgram, list [pair (s "name") (s "a")]]) `interShouldBe` prog (Program [VarName "namelist",VarName "valuelist"] [BasicBlock {label = Label "[\"cont\",[]]", assigns = [Assignment (VarName "namelist") (Tl (Var (VarName "namelist"))),Assignment (VarName "valuelist") (Tl (Var (VarName "valuelist")))], jmp = Goto (Label "search")}])

descrToProgProgram :: Program
descrToProgProgram =
  program
    ["program", "staticVars"]
    [ bb "init" [] $ ret (descrToProg "program" "staticVars" searchContDescr)
    ]

searchContDescr :: Expr
searchContDescr =
  list
    [ list
        [ list [s "goto", s "search"],
          list [s "assign", s "valuelist", expr (tl "valuelist")],
          list [s "assign", s "namelist", expr (tl "namelist")],
          pair (s "cont") (list [])
        ]
    ]

mixedSearch :: Program
mixedSearch =
  program
    ["valuelist"]
    [ bb
        "[\"init\",[[\"name\",\"c\"],[\"namelist\",[\"b\",\"a\",\"c\"]]]]"
        [ "valuelist" @= tl "valuelist",
          "valuelist" @= tl "valuelist"
        ]
        $ ret
        $ hd "valuelist"
    ]

searchProgram :: Program
searchProgram =
  program
    ["name", "namelist", "valuelist"]
    [ bb "init" [] $ jump "search",
      bb "search" [] $ jumpc ("name" == hd "namelist") "found" "cont",
      bb
        "cont"
        [ "valuelist" @= tl "valuelist",
          "namelist" @= tl "namelist"
        ]
        $ jump "search",
      bb "found" [] $ ret (hd "valuelist")
    ]
