{-# LANGUAGE OverloadedStrings #-}

module Flowchart.InterpreterSpec (interpreterSpec) where

import Flowchart.DSL
import Flowchart.TestPrograms
import Test.Hspec
import TestUtils
import Prelude hiding ((+), (==))

interpreterSpec :: Spec
interpreterSpec = describe "Flowchart Interpreter" $ do
  spec_basic
  spec_case
  spec_list
  spec_map
  spec_reduce
  spec_commands

spec_basic :: Spec
spec_basic =
  describe "basic" $ do
    it "interpretes return 2" $
      (returnTwo, []) `interShouldBe` int 2
    it "interpretes loop" $
      (loop, [int 1]) `interShouldBe` int 10
    it "interpretes pair" $
      (swapPair, []) `interShouldBe` cons (int 2) (int 1)
    it "interpretes string" $
      (returnStr, []) `interShouldBe` s "str"
    it "interpretes suffixFrom" $
      (suffixFromProgram, [])
        `interShouldBe` list [s "b", s "c", s "d"]

spec_case :: Spec
spec_case =
  describe "case" $ do
    it "not default case" $
      (caseProgram, [s "write"]) `interShouldBe` s "write"
    it "default case" $
      (caseProgram, [s "_"]) `interShouldBe` s "error"

spec_list :: Spec
spec_list =
  describe "list" $ do
    it "interpretes list" $
      (returnList, []) `interShouldBe` list [int 1, int 2]
    it "interpretes indexOf" $
      (indexOf, [list [s "a", s "b", s "c"], s "b"]) `interShouldBe` int 1
    it "interpretes member when element is found" $
      (memberProgram, [s "b"]) `interShouldBe` true
    it "interpretes member when element is not found" $
      (memberProgram, [s "e"]) `interShouldBe` false

spec_map :: Spec
spec_map = describe "map" $ do
  it "interpretes insert" $
    (insertProgram, [s "b"]) `interShouldBe` list [cons (s "b") (int 2), cons (s "a") (int 11), cons (cons (s "a") (int 10)) unit]
  it "interpretes lookup" $
    (lookupProgram, [cons (s "c") (int 10)]) `interShouldBe` s "cv"
  it "interpretes lookup when value not found" $
    (lookupProgram, [cons (s "d") (int 10)]) `interShouldBe` unit

spec_reduce :: Spec
spec_reduce = describe "reduce" $ do
  it "interpretes reduce" $
    (reduceProgram, [list [cons (s "x") (int 2)]]) `interShouldBe` expr (int 5 + "y")
  it "interpretes eval" $
    (evalProgram, [list [cons (s "x") (int 2), cons (s "y") (int 4)]]) `interShouldBe` int 9

spec_commands :: Spec
spec_commands = describe "commands" $ do
  it "interpretes commands loop 'loop'" $
    (commandsProgram, [prog loop, s "loop"])
      `interShouldBe` list
        [ list [s "assign", s "x", expr ("x" + int 1)],
          list [s "if", expr ("x" == int 10), s "ret", s "loop"]
        ]
  it "interpretes commands loop 'ret'" $
    (commandsProgram, [prog loop, s "ret"])
      `interShouldBe` list
        [ list [s "return", expr "x"]
        ]
  it "interpretes commands loop 'ret'" $
    (commandsProgram, [prog insertProgram, s "init"])
      `interShouldBe` list
        [ list [s "assign", s "m", expr $ list [cons (s "b") (int 3), cons (s "a") (int 11)]],
          list [s "assign", s "m", expr $ insert "m" (cons (s "a") (int 10)) unit],
          list [s "assign", s "m", expr $ insert "m" "k" (int 2)],
          list [s "return", expr "m"]
        ]
