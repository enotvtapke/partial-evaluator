module Turing.TestPrograms (replaceFirstOne) where
import Flowchart.AST
import Flowchart.DSL

replaceFirstOne :: Expr
replaceFirstOne = list [
    list [s "if", int 0, int 3],
    list [s "right"],
    list [s "goto", int 0],
    list [s "write", int 1]
  ]
