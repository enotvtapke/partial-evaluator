{-# LANGUAGE OverloadedStrings #-}

module Turing.InterpreterSpec (interpreterSpec) where

import Flowchart.DSL
import Test.Hspec
import TestUtils
import Turing.Interpreter
import Turing.TestPrograms
import Prelude hiding ((+), (==))

interpreterSpec :: Spec
interpreterSpec = describe "Turing Machine Interpreter" $ do
  spec_basic

spec_basic :: Spec
spec_basic =
  describe "basic" $ do
    it "interpretes replace first One" $
      (turingInterpreter, [replaceFirstOne, list [int 1, int 1, int 0, int 1, int 0, int 1]])
        `interShouldBe` list [int 1, int 1, int 0, int 1]
