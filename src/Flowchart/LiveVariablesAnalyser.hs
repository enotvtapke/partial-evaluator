module Flowchart.LiveVariablesAnalyser (liveVariables) where

import Control.Monad.Identity (Identity (runIdentity))
import qualified Data.HashMap.Internal.Strict as M
import Data.List (group, sort, union, (\\))
import Flowchart.AST
import Flowchart.DivisionCalculator (exprVars)

type PointId = (Label, Int)

data Point = Point {pointId :: PointId, pointDefs :: [VarName], pointRefs :: [VarName], successorsIds :: [PointId]}

liveVariables :: Program -> M.HashMap Label [VarName]
liveVariables (Program _ blocks) = M.mapKeys fst $ M.filterWithKey (\pid _ -> snd pid == 0) $ lives M.empty (concatMap blockToPoints blocks)

lives :: M.HashMap PointId [VarName] -> [Point] -> M.HashMap PointId [VarName]
lives liveVarsPerPoint points = runIdentity $ do
  let liveVarsPerPoint' = foldr updateLiveVars liveVarsPerPoint points
  return $ if liveVarsPerPoint' == liveVarsPerPoint then liveVarsPerPoint else lives liveVarsPerPoint' points
  where
    updateLiveVars :: Point -> M.HashMap PointId [VarName] -> M.HashMap PointId [VarName]
    updateLiveVars p liveVars = M.insert (pointId p) (live p (\k -> M.lookupDefault [] k liveVars)) liveVars

live :: Point -> (PointId -> [VarName]) -> [VarName]
live point f = (successorsLive \\ pointDefs point) `union` pointRefs point
  where
    successorsLive = unique $ concatMap f (successorsIds point)

blockToPoints :: BasicBlock -> [Point]
blockToPoints (BasicBlock lab assigns jmp) = runIdentity $ do
  let assignsPoints = zipWith (\i (Assignment varName varExpr) -> Point (lab, i) [varName] (refs varExpr) [(lab, i + 1)]) [0 .. length assigns - 1] assigns
  let jmpPoint = case jmp of
        Goto l1 -> Point (lab, length assignsPoints) [] [] [(l1, 0)]
        If c l1 l2 -> Point (lab, length assignsPoints) [] (refs c) [(l1, 0), (l2, 0)]
        Return e -> Point (lab, length assignsPoints) [] (refs e) []
  return $ assignsPoints ++ [jmpPoint]

refs :: Expr -> [VarName]
refs e = unique $ exprVars e

unique :: (Ord a) => [a] -> [a]
unique a = map head (group (sort a))
