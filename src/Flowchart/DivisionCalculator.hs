module Flowchart.DivisionCalculator (programStaticVars', programStaticVars, exprIsStatic, blockVars, exprVars) where

import Control.Monad (join)
import Data.List (isSubsequenceOf, sort, group, intersect, (\\))
import Flowchart.AST

varNamesToList :: [VarName] -> Value
varNamesToList variables = List (map (\(VarName name) -> StringLiteral name) variables)

programStaticVars' :: Program -> [VarName] -> [VarName]
programStaticVars' p@(Program initVars _) staticInitVars = programVars p \\ programDynamicVars' p (initVars \\ staticInitVars)

programStaticVars :: Program -> [String] -> Expr
programStaticVars p staticVars = Constant $ varNamesToList $ programStaticVars' p (map VarName staticVars)

exprIsStatic :: Expr -> [VarName] -> Bool
exprIsStatic e staticVars = isSubsequenceOf (sort (exprVars e)) (sort staticVars)

programVars :: Program -> [VarName]
programVars (Program initVars blocks) = (map head . group . sort) (initVars ++ join (map blockVars blocks))

blockVars :: BasicBlock -> [VarName]
blockVars (BasicBlock _ assgns _) = map (\(Assignment vn _) -> vn) assgns

programDynamicVars' :: Program -> [VarName] -> [VarName]
programDynamicVars' p@(Program _ blocks) dynamicVars =
  let dynamicVars' = (map head . group . sort) $ dynamicVars ++ join (map (`blockDynamicVars` dynamicVars) blocks)
   in if dynamicVars == dynamicVars' then dynamicVars else programDynamicVars' p dynamicVars'
  where
    blockDynamicVars :: BasicBlock -> [VarName] -> [VarName]
    blockDynamicVars (BasicBlock _ assgns _) dynamicVars =
      map (\(Assignment vn _) -> vn) $
        filter (\(Assignment _ e) -> exprIsDynamic e dynamicVars) assgns

exprIsDynamic :: Expr -> [VarName] -> Bool
exprIsDynamic e dynamicVars = (exprVars e `intersect` dynamicVars) /= []

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
exprVars (DynamicLabels e1 e2) = exprVars e1 ++ exprVars e2
exprVars (CompressLabels e1 e2) = exprVars e1 ++ exprVars e2
exprVars (Or e1 e2) = exprVars e1 ++ exprVars e2
