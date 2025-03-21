{-# LANGUAGE TupleSections #-}

module Flowchart.LiveVariablesAnalyser (liveVariables) where

import Control.Monad.State.Lazy
import qualified Data.HashMap.Internal.Strict as M
import Data.List (nub, sort, union, (\\))
import Flowchart.AST
import Flowchart.DivisionCalculator (blockVars, exprVars)
import Flowchart.Mix (mix)
import Text.Printf (printf)

blocksToMap :: [BasicBlock] -> M.HashMap Label BasicBlock
blocksToMap bbs = M.fromList $ map (\b@(BasicBlock l _ _) -> (l, b)) bbs

referrences :: BasicBlock -> [VarName]
referrences (BasicBlock _ assgns _) = nub $ join $ map (\(Assignment _ expr) -> exprVars expr) assgns

definitions :: BasicBlock -> [VarName]
definitions = nub . blockVars

successors :: BasicBlock -> [Label]
successors (BasicBlock _ _ jump) =
  case jump of
    Goto l -> [l]
    If _ l1 l2 -> [l1, l2]
    Return _ -> []

type LiveMonad = State (M.HashMap Label [VarName])

live :: M.HashMap Label BasicBlock -> Label -> LiveMonad ()
live labelToBlock lab = do
  successorsLive <- join <$> mapM (\l -> gets (`unsafeLookup` l)) (successors block)
  modify (M.insert lab ((nub successorsLive \\ definitions block) `union` referrences block))
  where
    block = unsafeLookup labelToBlock lab
    unsafeLookup m k = case m M.!? k of
      Just v -> v
      Nothing -> error $ printf "Key `%s` not found" (show k)

lives :: M.HashMap Label BasicBlock -> LiveMonad (M.HashMap Label [VarName])
lives labelToBlock = do
  living <- get
  mapM_ (live labelToBlock) (sort (M.keys labelToBlock))
  living' <- get
  if living == living' then return living' else lives labelToBlock

liveVariables :: Program -> M.HashMap Label [VarName]
liveVariables (Program _ blocks) = evalState (lives (blocksToMap blocks)) (M.fromList $ map (,[]) (M.keys $ blocksToMap blocks))
