module Flowchart.Interpreter.EvalState
  ( EvalMonad,
    Error (..),
    putLabel,
    putVar,
    getLabel,
    getVar,
    runEvalMonad,
    EvalState(..),
    getVarMaybe,
  )
where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.HashMap.Lazy as M
import Flowchart.AST
import GHC.IO (unsafePerformIO)

data EvalState = EvalState {vars :: M.HashMap VarName Value, labels :: M.HashMap Label BasicBlock}

data Error
  = Error
  | EmptyProgram
  | NotReducedExpressionEvaluation Expr
  | UndefinedVariable VarName
  | UndefinedLabel Label
  | UnimplementedExpr Expr
  | IncorrectType String
  | IncorrectArgsTypes [Value] String
  | IndexOutOfBounds String
  | InvalidStaticVars [VarName]
  deriving (Show, Eq)

type EvalMonad = StateT EvalState (ExceptT Error IO)

runEvalMonad :: EvalMonad a -> Either Error a
runEvalMonad m = unsafePerformIO $ runExceptT $ evalStateT m (EvalState M.empty M.empty)

putLabel :: Label -> BasicBlock -> EvalMonad ()
putLabel l bb = modify (\st -> EvalState (vars st) (M.insert l bb (labels st)))

putVar :: VarName -> Value -> EvalMonad ()
putVar var value = modify (\st -> EvalState (M.insert var value $ vars st) (labels st))

getLabel :: Label -> EvalMonad BasicBlock
getLabel l = do
  s <- gets labels
  case M.lookup l s of
    Just x -> return x
    Nothing -> lift $ throwE $ UndefinedLabel l

getVar :: VarName -> EvalMonad Value
getVar varName = do
  vs <- gets vars
  case M.lookup varName vs of
    Just x -> return x
    Nothing -> lift $ throwE $ UndefinedVariable varName

getVarMaybe :: VarName -> EvalMonad (Maybe Value)
getVarMaybe varName = do
  vs <- gets vars
  return $ M.lookup varName vs
