module Flowchart.Interpreter.Interpreter (interpret, interpretValues, evalExpr) where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except (throwE)
import Data.List (uncons)
import Flowchart.AST
import Flowchart.Interpreter.EvalState

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
    _ -> lift $ throwE $ IncorrectValuesTypes [condVal] "in if condition"

evalExpr :: Expr -> EvalMonad Value
evalExpr (Constant v) = return v
evalExpr (Var name) = getVar name
evalExpr (Eq e1 e2) = evalBinExpr e1 e2 eq
evalExpr (Plus e1 e2) = evalBinExpr e1 e2 plus
evalExpr (Car e) = evalUnExpr e car
evalExpr (Cdr e) = evalUnExpr e cdr
evalExpr (Cons e1 e2) = evalBinExpr e1 e2 cons
evalExpr (SuffixFrom l i) = evalBinExpr l i suffixFrom

evalBinExpr :: Expr -> Expr -> (Value -> Value -> EvalMonad Value) -> EvalMonad Value
evalBinExpr e1 e2 f = do
  e1Val <- evalExpr e1
  ev2Val <- evalExpr e2
  f e1Val ev2Val

evalUnExpr :: Expr -> (Value -> EvalMonad Value) -> EvalMonad Value
evalUnExpr e f = evalExpr e >>= f

plus :: Value -> Value -> EvalMonad Value
plus (IntLiteral x) (IntLiteral y) = return $ IntLiteral $ x + y
plus x y = lift $ throwE $ IncorrectValuesTypes [x, y] "in `plus` args"

eq :: Value -> Value -> EvalMonad Value
eq x y = return $ BoolLiteral $ x == y

car :: Value -> EvalMonad Value
car (Pair x _) = return x
car x = lift $ throwE $ IncorrectValuesTypes [x] "in `car` args"

cdr :: Value -> EvalMonad Value
cdr (Pair _ y) = return y
cdr x = lift $ throwE $ IncorrectValuesTypes [x] "in `cdr` args"

cons :: Value -> Value -> EvalMonad Value
cons x y = return $ Pair x y

suffixFrom :: Value -> Value -> EvalMonad Value
suffixFrom p@(Pair _ _) (IntLiteral 0) = return p
suffixFrom (Pair _ xs) (IntLiteral n) = suffixFrom xs (IntLiteral (n - 1))
suffixFrom (Pair _ Unit) _ = lift $ throwE $ IndexOutOfBounds "in `suffixFrom`"
suffixFrom l x = lift $ throwE $ IncorrectValuesTypes [l, x] "in `suffixFrom` args"

-- debugWrite :: String -> RunMonad ()
-- debugWrite s = lift $ lift $ putStrLn s
