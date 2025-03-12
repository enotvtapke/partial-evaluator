module Flowchart.Interpreter (runFull, runFullExpr) where

import Flowchart.AST
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.HashMap.Lazy as M
import Data.List (uncons)
import GHC.IO (unsafePerformIO)

data RunState = RunState {vars :: M.HashMap VarName Value, labels :: M.HashMap Label BasicBlock}

data Error
  = Error
  | EmptyProgram
  | UndefinedVariable VarName
  | UndefinedLabel Label
  | UnimplementedExpr Expr
  | IncorrectType String
  | IncorrectValuesTypes [Value] String
  | IndexOutOfBounds String
  deriving (Show, Eq)

type RunMonad = StateT RunState (ExceptT Error IO)

putLabel :: Label -> BasicBlock -> RunMonad ()
putLabel l bb = modify (\st -> RunState (vars st) (M.insert l bb (labels st)))

putVar :: VarName -> Value -> RunMonad ()
putVar var value = modify (\st -> RunState (M.insert var value $ vars st) (labels st))

getLabel :: Label -> RunMonad BasicBlock
getLabel l = do
  s <- gets labels
  case M.lookup l s of
    Just x -> return x
    Nothing -> lift $ throwE $ UndefinedLabel l

getVar :: VarName -> RunMonad Value
getVar varName = do
  vs <- gets vars
  case M.lookup varName vs of
    Just x -> return x
    Nothing -> lift $ throwE $ UndefinedVariable varName

runFull :: Program -> [Value] -> Either Error Value
runFull p v = unsafePerformIO $ runExceptT $ evalStateT (run p v) (RunState M.empty M.empty)

run :: Program -> [Value] -> RunMonad Value
run (Program names body) values = do
  put $ RunState (M.fromList $ zip names values) M.empty
  putLabels body
  maybe (lift $ throwE EmptyProgram) (evalBlock . fst) (uncons body)

runFullExpr :: Program -> [Expr] -> Either Error Value
runFullExpr p v = unsafePerformIO $ runExceptT $ evalStateT (runExpr p v) (RunState M.empty M.empty)

runExpr :: Program -> [Expr] -> RunMonad Value
runExpr (Program names body) exprs = do
  values <- mapM evalExpr exprs 
  put $ RunState (M.fromList $ zip names values) M.empty
  putLabels body
  maybe (lift $ throwE EmptyProgram) (evalBlock . fst) (uncons body)

putLabels :: [BasicBlock] -> RunMonad ()
putLabels = mapM_ (\bb@(BasicBlock l _ _) -> putLabel l bb)

evalBlock :: BasicBlock -> RunMonad Value
evalBlock (BasicBlock _ assgn jmp) = do
  mapM_ evalAssignment assgn
  evalJump jmp

evalAssignment :: Assignment -> RunMonad ()
evalAssignment (Assignment name expr) = evalExpr expr >>= putVar name

evalJump :: Jump -> RunMonad Value
evalJump (Goto l) = getLabel l >>= evalBlock
evalJump (Return e) = evalExpr e
evalJump (If cond l1 l2) = do
  condVal <- evalExpr cond
  case condVal of
    BoolLiteral True -> getLabel l1 >>= evalBlock
    BoolLiteral False -> getLabel l2 >>= evalBlock
    _ -> lift $ throwE $ IncorrectValuesTypes [condVal] "in if condition"

evalExpr :: Expr -> RunMonad Value
evalExpr (Constant v) = return v
evalExpr (Var name) = getVar name
evalExpr (Eq e1 e2) = evalBinExpr e1 e2 eq
evalExpr (Plus e1 e2) = evalBinExpr e1 e2 plus
evalExpr (Car e) = evalUnExpr e car
evalExpr (Cdr e) = evalUnExpr e cdr
evalExpr (Cons e1 e2) = evalBinExpr e1 e2 cons
evalExpr (SuffixFrom l i) = evalBinExpr l i suffixFrom

evalBinExpr :: Expr -> Expr -> (Value -> Value -> RunMonad Value) -> RunMonad Value
evalBinExpr e1 e2 f = do
  e1Val <- evalExpr e1
  ev2Val <- evalExpr e2
  f e1Val ev2Val

evalUnExpr :: Expr -> (Value -> RunMonad Value) -> RunMonad Value
evalUnExpr e f = evalExpr e >>= f

plus :: Value -> Value -> RunMonad Value
plus (IntLiteral x) (IntLiteral y) = return $ IntLiteral $ x + y
plus x y = lift $ throwE $ IncorrectValuesTypes [x, y] "in `plus` args"

eq :: Value -> Value -> RunMonad Value
eq x y = return $ BoolLiteral $ x == y

car :: Value -> RunMonad Value
car (Pair x _) = return x
car x = lift $ throwE $ IncorrectValuesTypes [x] "in `car` args"

cdr :: Value -> RunMonad Value
cdr (Pair _ y) = return y
cdr x = lift $ throwE $ IncorrectValuesTypes [x] "in `cdr` args"

cons :: Value -> Value -> RunMonad Value
cons x y = return $ Pair x y

suffixFrom :: Value -> Value -> RunMonad Value
suffixFrom p@(Pair _ _) (IntLiteral 0) = return p
suffixFrom (Pair _ xs) (IntLiteral n) = suffixFrom xs (IntLiteral (n - 1))
suffixFrom (Pair _ Unit) _ = lift $ throwE $ IndexOutOfBounds "in `suffixFrom`"
suffixFrom l x = lift $ throwE $ IncorrectValuesTypes [l, x] "in `suffixFrom` args"

-- debugWrite :: String -> RunMonad ()
-- debugWrite s = lift $ lift $ putStrLn s
