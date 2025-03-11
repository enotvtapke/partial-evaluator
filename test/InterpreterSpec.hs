{-# LANGUAGE OverloadedStrings #-}

module InterpreterSpec (interpreterSpec) where

import Flowchart.AST
import Flowchart.Interpreter (runFull)
import Test.Hspec
import TestPrograms
import Prelude hiding ((+), (==))

interpreterSpec :: Spec
interpreterSpec = describe "Interpreter" $ do
  spec_basic
  spec_case
  spec_list

spec_basic :: Spec
spec_basic =
  describe "basic" $ do
    it "interpretes return 2" $
      runFull returnTwo [] `shouldBe` Right (IntLiteral 2)
    it "interpretes loop" $
      runFull loop [IntLiteral 1] `shouldBe` Right (IntLiteral 10)
    it "interpretes pair" $
      runFull swapPair [] `shouldBe` Right (Pair (IntLiteral 2) (IntLiteral 1))
    it "interpretes string" $
      runFull returnStr [] `shouldBe` Right (StringLiteral "str")

spec_case :: Spec
spec_case =
  describe "case" $ do
    it "not default case" $
      runFull caseProgram [StringLiteral "write"] `shouldBe` Right (StringLiteral "write")
    it "default case" $
      runFull caseProgram [StringLiteral "_"] `shouldBe` Right (StringLiteral "error")

spec_list :: Spec
spec_list =
  describe "list" $ do
    it "interpretes list" $
      runFull returnList [] `shouldBe` Right (Pair (IntLiteral 1) (Pair (IntLiteral 2) Unit))
    it "interpretes indexOf" $
      runFull
        indexOf
        [Pair (StringLiteral "a") (Pair (StringLiteral "b") (Pair (StringLiteral "c") Unit)), StringLiteral "b"]
        `shouldBe` Right (IntLiteral 1)
