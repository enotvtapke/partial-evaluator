{-# LANGUAGE OverloadedStrings #-}

module Flowchart.MixSpec (mixSpec) where

import Flowchart.AST
import Flowchart.DSL
import Flowchart.Mix
import Test.Hspec
import TestUtils
import Prelude hiding ((+), (==))

mixSpec :: Spec
mixSpec = describe "Flowchart Mix" $ do
  spec_search
  spec_descrToProg

spec_search :: Spec
spec_search =
  describe "search" $ do
    it "mixes search" $
      (mix, [prog searchProgram, list [cons (s "name") (s "c"), cons (s "namelist") $ list [s "b", s "a", s "c"]]]) `interShouldBe` int 2
    it "interpretes search" $
      (searchProgram, [s "a", list [s "b", s "a", s "c"], list [s "bv", s "av", s "cv"]]) `interShouldBe` s "av"

spec_descrToProg :: Spec
spec_descrToProg =
  describe "descrToProg" $ do
    it "interpretes descrToProg" $
      (descrToProgProgram, [prog searchProgram, list []]) `interShouldBe` int 2

descrToProgProgram :: Program
descrToProgProgram =
  program
    ["program", "staticVars"]
    [ bb "init" [] $ ret (descrToProg "program" "staticVars" $ list [commands "program" (s "cont")])
    ]

searchProgram :: Program
searchProgram =
  program
    ["name", "namelist", "valuelist"]
    [ bb "init" [] $ jump "search",
      bb "search" [] $ jumpc ("name" == car "namelist") "found" "cont",
      bb
        "cont"
        [ "valuelist" @= cdr "valuelist",
          "namelist" @= cdr "namelist"
        ]
        $ jump "search",
      bb "found" [] $ ret (car "valuelist")
    ]
