module Flowchart.DivisionCalculator (programStaticVars', programStaticVars, exprIsStatic) where

import Control.Monad (join)
import Data.List (isSubsequenceOf, sort, group, nub)
import Flowchart.AST

programStaticVars :: Program -> [String] -> Expr
programStaticVars p names = Constant $ varNamesToList $ programStaticVars' p (map VarName names)

varNamesToList :: [VarName] -> Value
varNamesToList variables = List (map (\(VarName name) -> StringLiteral name) variables)

programStaticVars' :: Program -> [VarName] -> [VarName]
programStaticVars' p@(Program _ blocks) staticVars =
  nub $ let staticVars' = (map head . group . sort) $ staticVars ++ join (map (`blockStaticVars` staticVars) blocks)
   in if staticVars == staticVars' then staticVars else programStaticVars' p staticVars'

blockStaticVars :: BasicBlock -> [VarName] -> [VarName]
blockStaticVars (BasicBlock _ assgns _) staticVars =
  map (\(Assignment vn _) -> vn) $
    filter (\(Assignment _ e) -> exprIsStatic e staticVars) assgns

exprIsStatic :: Expr -> [VarName] -> Bool
exprIsStatic e staticVars = isSubsequenceOf (sort (exprVars e)) (sort staticVars)

exprVars :: Expr -> [VarName]
exprVars (Constant _) = []
exprVars (Var name) = [name]
exprVars (Eq e1 e2) = exprVars e1 ++ exprVars e2
exprVars (Plus e1 e2) = exprVars e1 ++ exprVars e2
exprVars (Hd e1) = exprVars e1
exprVars (Tl e1) = exprVars e1
exprVars (Cons e1 e2) = exprVars e1 ++ exprVars e2
exprVars (SuffixFrom e1 e2) = exprVars e1 ++ exprVars e2
exprVars (Member e1 e2) = exprVars e1 ++ exprVars e2
exprVars (Insert e1 e2 e3) = exprVars e1 ++ exprVars e2 ++ exprVars e3
exprVars (Lookup e1 e2) = exprVars e1 ++ exprVars e2
exprVars (Commands e1 e2) = exprVars e1 ++ exprVars e2
exprVars (Eval e1 e2) = exprVars e1 ++ exprVars e2
exprVars (Reduce e1 e2) = exprVars e1 ++ exprVars e2
exprVars (IsStatic e1 e2) = exprVars e1 ++ exprVars e2
exprVars (TraceExpr _ e2) = exprVars e2
exprVars (DescrToProg e1 e2 e3) = exprVars e1 ++ exprVars e2 ++ exprVars e3
exprVars (ToLabel e1) = exprVars e1
exprVars (DynamicLabels e1) = exprVars e1
exprVars (CompressLabels e1 e2) = exprVars e1 ++ exprVars e2
exprVars (Or e1 e2) = exprVars e1 ++ exprVars e2
