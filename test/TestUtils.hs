module TestUtils (interShouldBe, runProgram) where

import Flowchart.AST
import Flowchart.Interpreter.Interpreter
import Flowchart.Interpreter.EvalState
import Test.Hspec (Expectation, shouldBe)

interShouldBe :: (Program, [Expr]) -> Expr -> Expectation
(p, args) `interShouldBe` expected = runEvalMonad (interpret p args) `shouldBe` runEvalMonad (evalExpr expected)

runProgram :: Program -> [Expr] -> Program
runProgram p args = case runEvalMonad (interpret p args) of
  Right (Prog p') -> p'
  x -> error (show x)
