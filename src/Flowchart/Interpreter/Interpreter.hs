module Flowchart.Interpreter.Interpreter (interpret, interpretValues, evalExpr, putLabels) where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except (throwE)
import Data.List (uncons, find)
import Flowchart.AST
import Flowchart.Interpreter.EvalState
import Flowchart.DSL (ev, sv, listv)
import Prelude hiding (lookup)

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
evalExpr (Constant v) = return v
evalExpr (Var name) = getVar name
evalExpr (Eq e1 e2) = evalBinExpr e1 e2 eq
evalExpr (Plus e1 e2) = evalBinExpr e1 e2 plus
evalExpr (Car e) = evalUnExpr e car
evalExpr (Cdr e) = evalUnExpr e cdr
evalExpr (Cons e1 e2) = evalBinExpr e1 e2 cons
evalExpr (SuffixFrom l i) = evalBinExpr l i suffixFrom
evalExpr (Member l i) = evalBinExpr l i member
evalExpr (Insert m k v) = evalTerExpr m k v insert
evalExpr (Lookup m k) = evalBinExpr m k lookup
evalExpr (Commands p l) = evalBinExpr p l commands

evalBinExpr :: Expr -> Expr -> (Value -> Value -> EvalMonad Value) -> EvalMonad Value
evalBinExpr e1 e2 f = do
  e1Val <- evalExpr e1
  ev2Val <- evalExpr e2
  f e1Val ev2Val

evalTerExpr :: Expr -> Expr -> Expr -> (Value -> Value -> Value -> EvalMonad Value) -> EvalMonad Value
evalTerExpr e1 e2 e3 f = do
  e1Val <- evalExpr e1
  ev2Val <- evalExpr e2
  ev3Val <- evalExpr e3
  f e1Val ev2Val ev3Val

evalUnExpr :: Expr -> (Value -> EvalMonad Value) -> EvalMonad Value
evalUnExpr e f = evalExpr e >>= f

plus :: Value -> Value -> EvalMonad Value
plus (IntLiteral x) (IntLiteral y) = return $ IntLiteral $ x + y
plus x y = lift $ throwE $ IncorrectArgsTypes [x, y] "in `plus` args"

eq :: Value -> Value -> EvalMonad Value
eq x y = return $ BoolLiteral $ x == y

car :: Value -> EvalMonad Value
car (Pair x _) = return x
car x = lift $ throwE $ IncorrectArgsTypes [x] "in `car` args"

cdr :: Value -> EvalMonad Value
cdr (Pair _ y) = return y
cdr x = lift $ throwE $ IncorrectArgsTypes [x] "in `cdr` args"

cons :: Value -> Value -> EvalMonad Value
cons x y = return $ Pair x y

suffixFrom :: Value -> Value -> EvalMonad Value
suffixFrom p@(Pair _ _) (IntLiteral 0) = return p
suffixFrom (Pair _ xs) (IntLiteral n) = suffixFrom xs (IntLiteral (n - 1))
suffixFrom (Pair _ Unit) _ = lift $ throwE $ IndexOutOfBounds "in `suffixFrom`"
suffixFrom l x = lift $ throwE $ IncorrectArgsTypes [l, x] "in `suffixFrom` args"

member :: Value -> Value -> EvalMonad Value
member (Pair x _) e | e == x = return $ BoolLiteral True
member (Pair _ Unit) _ = return $ BoolLiteral False
member (Pair _ xs) e = member xs e
member p e = lift $ throwE $ IncorrectArgsTypes [p, e] "in `member` args"

insert :: Value -> Value -> Value -> EvalMonad Value
insert (Pair (Pair k _) ms) k1 v | k1 == k = return $ Pair (Pair k1 v) ms
insert Unit k v = return $ Pair (Pair k v) Unit
insert (Pair p ms) k v = Pair p <$> insert ms k v
insert p k v = lift $ throwE $ IncorrectArgsTypes [p, k, v] "in `insert` args"

lookup :: Value -> Value -> EvalMonad Value
lookup (Pair (Pair k v) _) k1 | k1 == k = return v
lookup Unit _ = return Unit
lookup (Pair (Pair _ _) ms) k = lookup ms k
lookup p k = lift $ throwE $ IncorrectArgsTypes [p, k] "in `lookup` args"


-- debugWrite :: String -> RunMonad ()
-- debugWrite s = lift $ lift $ putStrLn s

-- Built-in functions for mix

commands :: Value -> Value -> EvalMonad Value
commands (Prog (Program _ body)) (StringLiteral l) = commandsByBlock <$> findBlock (Label l) body
  where
    findBlock :: Label -> [BasicBlock] -> EvalMonad BasicBlock
    findBlock ll blocks = maybe (lift $ throwE Error) return (find (\(BasicBlock l1 _ _) -> ll Prelude.== l1) blocks)
    commandsByBlock :: BasicBlock -> Value
    commandsByBlock (BasicBlock _ a j) = listv $ (assignToCommand <$> a) ++ [jumpToCommand j]
    assignToCommand :: Assignment -> Value
    assignToCommand (Assignment (VarName name) e) = listv [sv "assign", sv name, ev e]
    jumpToCommand :: Jump -> Value
    jumpToCommand (Goto (Label l)) = listv [sv "goto", sv l]
    jumpToCommand (If e (Label l1) (Label l2)) = listv [sv "if", ev e, sv l1, sv l2]
    jumpToCommand (Return c) = listv [sv "return", ev c]
commands x y = lift $ throwE $ IncorrectArgsTypes [x, y] "in commands"

-- eval :: Expr -> EvalMonad Value
-- reduce :: Expr -> EvalMonad Expr
-- toProgram :: Value -> Value (TExpr -> Prog)
-- add maps
