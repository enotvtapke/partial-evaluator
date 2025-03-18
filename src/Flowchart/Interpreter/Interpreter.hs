{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Flowchart.Interpreter.Interpreter
  ( interpret,
    interpretValues,
    evalExpr,
  )
where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except (throwE)
import qualified Data.HashMap.Lazy as M
import Data.List (uncons, sort, group, groupBy, nub)
import Flowchart.AST
import Flowchart.Interpreter.Builtin
import Flowchart.Interpreter.EvalState
import Prelude hiding (or, lookup)
import GHC.IO (unsafePerformIO)
import Control.Monad.Except (runExceptT)
import Debug.Trace (trace)

interpretValues :: Program -> [Value] -> EvalMonad Value
interpretValues p values = interpret p (Constant <$> values)

interpret :: Program -> [Expr] -> EvalMonad Value
interpret p@(Program names body) exprs = do
  values <- mapM evalExpr exprs
  mapM_ (uncurry putVar) (zip names values)
  putVar (VarName "_staticVars") (toValue $ staticVars p (M.fromList $ zip names values))
  putLabels body
  maybe (lift $ throwE EmptyProgram) (evalBlock . fst) (uncons body)

putLabels :: [BasicBlock] -> EvalMonad ()
putLabels = mapM_ (\bb@(BasicBlock l _ _) -> putLabel l bb)

evalBlock :: BasicBlock -> EvalMonad Value
evalBlock (BasicBlock l assgn jmp) = do
  v <- gets vars
  --  show (M.filterWithKey (\(VarName n) _ -> n == "residual") v)
  -- trace (show l) pure ()
  mapM_ evalAssignment assgn
  evalJump jmp

evalAssignment :: Assignment -> EvalMonad ()
evalAssignment (Assignment name expr) = evalExpr expr >>= putVar name

evalJump :: Jump -> EvalMonad Value
evalJump (Goto l) = getLabel l >>= evalBlock
evalJump (Return e) = evalExpr e
evalJump (If cond l1 l2) = do
  condVal <- evalExpr cond
  case condVal of
    BoolLiteral True -> getLabel l1 >>= evalBlock
    BoolLiteral False -> getLabel l2 >>= evalBlock
    _ -> lift $ throwE $ IncorrectArgsTypes [condVal] "in if condition"

evalExpr :: Expr -> EvalMonad Value
evalExpr e = reduceExpr e >>= exprToVal

exprToVal :: Expr -> EvalMonad Value
exprToVal (Constant ee) = return ee
exprToVal e = lift $ throwE $ NotReducedExpressionEvaluation e

reduceExpr :: Expr -> EvalMonad Expr
reduceExpr c@(Constant _) = return c
reduceExpr va@(Var name) = do
  v <- getVarMaybe name
  case v of
    Just x -> return (Constant x)
    Nothing -> return va
reduceExpr (Eq e1 e2) = reduceBinExpr e1 e2 Eq eq
reduceExpr (Plus e1 e2) = reduceBinExpr e1 e2 Plus plus
reduceExpr (Hd e) = reduceUnExpr e Hd hd
reduceExpr (Tl e) = reduceUnExpr e Tl tl
reduceExpr (Cons e1 e2) = reduceBinExpr e1 e2 Cons cons
reduceExpr (SuffixFrom l i) = reduceBinExpr l i SuffixFrom suffixFrom
reduceExpr (Member l i) = reduceBinExpr l i Member member
reduceExpr (Insert m k v) = reduceTerExpr m k v Insert insert
reduceExpr (Lookup m k) = reduceBinExpr m k Lookup lookup
reduceExpr (Commands p l) = reduceBinExpr p l Commands commands
reduceExpr (Eval e vars) = reduceBinExpr e vars Eval eval
reduceExpr (Reduce e vars) = reduceBinExpr e vars Reduce reduce
reduceExpr (IsStatic e vars) = reduceBinExpr e vars IsStatic isStatic
reduceExpr (TraceExpr traced e) = do
  tracedE <- reduceExpr traced
  List marked <- getVar $ VarName "marked"
  let trans = M.fromListWith (++) $ map (\(List [StringLiteral pp, (List vars)]) -> (pp, [M.fromListWith (++) $ map (\(List [StringLiteral vName, vVal]) -> (vName, [show vVal])) vars])) marked
  let pps = (map head . group . sort) $ map (\(List [StringLiteral pp, List vars]) -> pp) marked
  let varNames = (map head . group . sort) $ join $ map (\(List [StringLiteral pp, (List vars)]) -> map (\(List [StringLiteral vName, vVal]) -> vName) vars) marked
  let varVals = (map head . group . sort) $ map (\(List [StringLiteral pp, (List vars)]) -> (pp, map (\(List [StringLiteral vName, vVal]) -> (vName, show vVal)) vars)) marked
  let x = groupBy (\a b -> fst a == fst b) $ (map head . group . sort) $ join $ map (\(List [StringLiteral pp, (List vars)]) -> map (\(List [StringLiteral vName, vVal]) -> (vName, show vVal)) vars) marked
  let y = map (\l -> (fst $ head l, length l)) x
  -- let counts = M.mapKeys (\x -> fst $ head x) $ countElems x
  -- trace ("pps: " ++ show pps ++ "\n") pure ()
  -- trace ("varNames: " ++ show varNames ++ "\n") pure ()
  -- trace ("truncVarVals: " ++ show (trans M.!? "a") ++ "\n") pure ()
  -- trace ("truncVarVals: " ++ show (M.map (map (M.map (map length))) trans) ++ "\n") pure ()
  -- trace ("count: " ++ show y ++ "\n") pure ()
  -- trace ("marked:" ++ show (length $ nub marked)) pure ()
  -- trace ("marked:" ++ show (length marked)) pure ()
  -- trace ("marked1:" ++ show (length varVals)) pure ()
  -- trace ("marked1:" ++ show (length $ nub varVals)) pure ()
  trace (show tracedE) reduceExpr e
reduceExpr (DescrToProg prog stVars descr) = reduceTerExpr prog stVars descr DescrToProg descrToProg
reduceExpr (ToLabel l) = reduceUnExpr l ToLabel toLabel
reduceExpr (DynamicLabels p) = reduceUnExpr p ToLabel dynamicLabels
reduceExpr (CompressLabels prog initL) = reduceBinExpr prog initL CompressLabels compressLabels
reduceExpr (Or a b) = reduceBinExpr a b Or or

-- countElems :: (Hashable a) => [a] -> M.HashMap a Int
-- countElems = M.fromListWith (+) . flip zip (repeat 1)

reduceUnExpr :: Expr -> (Expr -> Expr) -> (Value -> EvalMonad Value) -> EvalMonad Expr
reduceUnExpr e c f =
  reduceExpr e
    >>= ( \case
            Constant x -> Constant <$> f x
            x -> return $ c x
        )

reduceBinExpr :: Expr -> Expr -> (Expr -> Expr -> Expr) -> (Value -> Value -> EvalMonad Value) -> EvalMonad Expr
reduceBinExpr e1 e2 c f = do
  e1Val <- reduceExpr e1
  e2Val <- reduceExpr e2
  case (e1Val, e2Val) of
    (Constant x, Constant y) -> Constant <$> f x y
    (x, y) -> return $ c x y

reduceTerExpr :: Expr -> Expr -> Expr -> (Expr -> Expr -> Expr -> Expr) -> (Value -> Value -> Value -> EvalMonad Value) -> EvalMonad Expr
reduceTerExpr e1 e2 e3 c f = do
  e1Val <- reduceExpr e1
  e2Val <- reduceExpr e2
  e3Val <- reduceExpr e3
  case (e1Val, e2Val, e3Val) of
    (Constant x, Constant y, Constant z) -> Constant <$> f x y z
    (x, y, z) -> return $ c x y z

-- Very special builtin functions for mix

isStatic :: Value -> Value -> EvalMonad Value
isStatic (Expr e) variables = do
  vs <- flip (M.!) (VarName "_staticVars") <$> gets vars 
  Expr er <- reduce (Expr e) vs
  return $ case er of
    Constant _ -> BoolLiteral True
    _ -> BoolLiteral False
isStatic x y = lift $ throwE $ IncorrectArgsTypes [x, y] "in `isStatic` args"

toValue :: M.HashMap VarName Value -> Value
toValue m = List $ map (\(VarName name, val) -> List [StringLiteral name, val]) (M.toList m)

staticVars :: Program -> M.HashMap VarName Value -> M.HashMap VarName Value
staticVars (Program _ bbs) vs0 = case runEvalMonad $ evalAssignsWithVars bbs vs0 of
  Right r -> r
  Left l -> error ("Error in `staticVars`: " ++ show l)
  where
    evalAssignsWithVars :: [BasicBlock] -> M.HashMap VarName Value -> EvalMonad (M.HashMap VarName Value)
    evalAssignsWithVars bbs variables = do
      modify (\(EvalState _ l) -> EvalState variables l)
      mapM_ evalAssigns bbs
      variables' <- gets vars
      if variables' == variables then return variables' else evalAssignsWithVars bbs variables'
      where
        evalAssigns :: BasicBlock -> EvalMonad ()
        evalAssigns (BasicBlock _ assigns _) =
          mapM_ (\(Assignment vName vExpr) -> do
              vExprReduced <- reduceExpr vExpr
              case vExprReduced of
                Constant v -> putVar vName v
                _ -> pure ()
            ) assigns


eval :: Value -> Value -> EvalMonad Value
eval (Expr e) vars = do
  Expr ee <- reduce (Expr e) vars
  exprToVal ee
eval x y = lift $ throwE $ IncorrectArgsTypes [x, y] "in `eval` args"

reduce :: Value -> Value -> EvalMonad Value
reduce (Expr e) vars = do
  vv <- varsToMap vars
  Expr <$> reduceInternal e vv
  where
    reduceInternal :: Expr -> M.HashMap VarName Value -> EvalMonad Expr
    reduceInternal e vars = case runWithVars (reduceExpr e) vars of
      Right v -> return v
      Left err -> lift $ throwE err
      where
        runWithVars m vs = unsafePerformIO $ runExceptT $ evalStateT m (EvalState vs M.empty)

    varsToMap :: Value -> EvalMonad (M.HashMap VarName Value)
    varsToMap (List l) = M.fromList <$> mapM go l
      where
        go :: Value -> EvalMonad (VarName, Value)
        go (List [StringLiteral k, v]) = return (VarName k, v)
        go x = lift $ throwE $ IncorrectArgsTypes [x] "in `varsToMap1` args"
    varsToMap p = lift $ throwE $ IncorrectArgsTypes [p] "in `varsToMap` args"
    
reduce x y = lift $ throwE $ IncorrectArgsTypes [x, y] "in `reduce` args"
