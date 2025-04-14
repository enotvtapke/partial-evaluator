module Main (main) where

import Flowchart.Mix (mix)
import Flowchart.DSL
import Flowchart.DivisionCalculator (programStaticVars)
import Flowchart.Interpreter.EvalState (runEvalMonad)
import Flowchart.Interpreter.Interpreter (interpret)
import Text.Printf (printf)

main :: IO ()
main = do
  let args = [prog mix, programStaticVars mix ["program", "staticVars"], list [pair (s "program") (prog mix), pair (s "staticVars") (programStaticVars mix ["program", "staticVars"])]]
  case runEvalMonad (interpret mix args) of
    Left e -> print e
    Right compilerGenerator -> printf "Third Futamura projection compiler generator for FlowChart:\n%s" (show compilerGenerator)
  return ()
