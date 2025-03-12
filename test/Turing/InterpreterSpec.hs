{-# LANGUAGE OverloadedStrings #-}

module Turing.InterpreterSpec (interpreterSpec) where

import Flowchart.AST
import Test.Hspec
import Prelude hiding ((+), (==))
import Turing.Interpreter
import Flowchart.Interpreter
import Flowchart.DSL

interpreterSpec :: Spec
interpreterSpec = describe "Turing Machine Interpreter" $ do
  spec_basic

spec_basic :: Spec
spec_basic =
  describe "basic" $ do
    it "interpretes replace first One" $
      runFullExpr turingInterpreter [replaceFirstOne, list [int 1, int 1, int 0, int 1, int 0, int 1]] `shouldBe` 
        Right (Pair (IntLiteral 1) $ Pair (IntLiteral 1) $ Pair (IntLiteral 0) $ Pair (IntLiteral 1) Unit)

replaceFirstOne :: Expr
replaceFirstOne = list [
    list [s "if", int 0, int 3],
    list [s "right"],
    list [s "goto", int 0],
    list [s "write", int 1]
  ]
