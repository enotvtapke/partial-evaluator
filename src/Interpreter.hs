module Interpreter (run, runFull) where

import AST
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
evalExpr e = lift $ throwE $ UnimplementedExpr e

evalBinExpr :: Expr -> Expr -> (Value -> Value -> RunMonad Value) -> RunMonad Value
evalBinExpr e1 e2 f = do
  e1Val <- evalExpr e1
  ev2Val <- evalExpr e2
  f e1Val ev2Val

plus :: Value -> Value -> RunMonad Value
plus (IntLiteral x) (IntLiteral y) = return $ IntLiteral $ x + y
plus x y = lift $ throwE $ IncorrectValuesTypes [x, y] "in plus args"

eq :: Value -> Value -> RunMonad Value
eq x y = return $ BoolLiteral $ x == y

debugWrite :: String -> RunMonad ()
debugWrite s = lift $ lift $ putStrLn s
