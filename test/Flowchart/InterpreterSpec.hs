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
