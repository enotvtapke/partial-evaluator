module Flowchart.DSL
  ( lab,
    bb,
    jumpc,
    jump,
    (Flowchart.DSL.+),
    (Flowchart.DSL.==),
    (@=),
    int,
    var,
    program,
    ret,
    cdr,
    car,
    cons,
    unit,
    list,
    s,
    suffixFrom,
  )
where

import Flowchart.AST

lab :: String -> Label
lab = Label

bb :: String -> [Assignment] -> Jump -> BasicBlock
bb l = BasicBlock (lab l)

jumpc :: Expr -> String -> String -> Jump
jumpc c l1 l2 = If c (lab l1) (lab l2)

jump :: String -> Jump
jump = Goto . lab

ret :: Expr -> Jump
ret = Return

infixl 6 +

(+) :: Expr -> Expr -> Expr
e1 + e2 = Plus e1 e2

infixl 4 ==

(==) :: Expr -> Expr -> Expr
e1 == e2 = Eq e1 e2

infix 1 @=

(@=) :: String -> Expr -> Assignment
x @= v = Assignment (VarName x) v

int :: Int -> Expr
int = Constant . IntLiteral

var :: String -> Expr
var = Var . VarName

cons :: Expr -> Expr -> Expr
cons = Cons

car :: Expr -> Expr
car = Car

cdr :: Expr -> Expr
cdr = Cdr

list :: [Expr] -> Expr
list = foldr cons unit

s :: String -> Expr
s = Constant . StringLiteral

suffixFrom :: Expr -> Expr -> Expr
suffixFrom = SuffixFrom

unit :: Expr
unit = Constant Unit

program :: [String] -> [BasicBlock] -> Program
program vars = Program (VarName <$> vars)
