module InterpreterSpec (interpreterSpec) where

import Test.Hspec
import Flowchart.AST
import Flowchart.DSL
import Flowchart.Interpreter (runFull)

import Prelude hiding ((+), (==))

interpreterSpec :: Spec
interpreterSpec = describe "Interpreter" $ do
  spec_basic

spec_basic :: Spec
spec_basic =
  describe "basic" $ do
    it "interpretes return 2" $
      runFull returnTwo [] `shouldBe` Right (IntLiteral 2)
    it "interpretes loop" $
      runFull loop [IntLiteral 1] `shouldBe` Right (IntLiteral 10)

returnTwo :: Program
returnTwo = program [] [
    bb "bb1"
    []
    $
    ret (int 2)
  ]

loop :: Program
loop = program ["x"] [
    bb "loop"
    ["x" @= var "x" + int 1]
    $
    jumpc (var "x" == int 10) "ret" "loop"
    ,
    bb "ret" [] $ ret $ var "x"
  ]
