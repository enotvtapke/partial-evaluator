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
import Data.List (uncons)
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
interpret (Program names body) exprs = do
  values <- mapM evalExpr exprs
  mapM_ (uncurry putVar) (zip names values)
  putLabels body
  maybe (lift $ throwE EmptyProgram) (evalBlock . fst) (uncons body)

putLabels :: [BasicBlock] -> EvalMonad ()
putLabels = mapM_ (\bb@(BasicBlock l _ _) -> putLabel l bb)

evalBlock :: BasicBlock -> EvalMonad Value
evalBlock (BasicBlock _ assgn jmp) = do
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
  trace (show tracedE) reduceExpr e
reduceExpr (DescrToProg prog stVars descr) = reduceTerExpr prog stVars descr DescrToProg descrToProg
reduceExpr (ToLabel l) = reduceUnExpr l ToLabel toLabel
reduceExpr (DynamicLabels p staticVarNames) = reduceBinExpr p staticVarNames DynamicLabels dynamicLabels
reduceExpr (CompressLabels prog initL) = reduceBinExpr prog initL CompressLabels compressLabels
reduceExpr (Or a b) = reduceBinExpr a b Or or

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
