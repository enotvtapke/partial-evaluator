{-# LANGUAGE LambdaCase #-}

module Flowchart.Interpreter.Builtin
  ( plus,
    eq,
    hd,
    tl,
    cons,
    suffixFrom,
    member,
    insert,
    lookup,
    commands,
    descrToProg,
    toLabel,
    compressLabels,
    or,
    dynamicLabels,
    isStatic,
    filterKeys,
    progLiveVariables,
  )
where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except (throwE)
import Data.Bifunctor (second)
import qualified Data.HashMap.Lazy as M
import Data.List (find, group, isSubsequenceOf, sort, (\\))
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Flowchart.AST
import Flowchart.DSL (ev, listv, sv)
import Flowchart.DivisionCalculator (exprIsStatic)
import Flowchart.Interpreter.EvalState
import Flowchart.LiveVariablesAnalyser (liveVariables)
import Prelude hiding (lookup, or)

plus :: Value -> Value -> EvalMonad Value
plus (IntLiteral x) (IntLiteral y) = return $ IntLiteral $ x + y
plus x y = lift $ throwE $ IncorrectArgsTypes [x, y] "in `plus` args"

eq :: Value -> Value -> EvalMonad Value
eq x y = return $ BoolLiteral $ x == y

hd :: Value -> EvalMonad Value
hd (List (x : _)) = return x
hd x = lift $ throwE $ IncorrectArgsTypes [x] "in `hd` args"

tl :: Value -> EvalMonad Value
tl (List (_ : xs)) = return $ List xs
tl x = lift $ throwE $ IncorrectArgsTypes [x] "in `tl` args"

cons :: Value -> Value -> EvalMonad Value
cons x (List l) = return $ List (x : l)
cons x y = lift $ throwE $ IncorrectArgsTypes [x, y] "in `cons` args"

suffixFrom :: Value -> Value -> EvalMonad Value
suffixFrom (List l) (IntLiteral n) = return $ List $ drop n l
suffixFrom l x = lift $ throwE $ IncorrectArgsTypes [l, x] "in `suffixFrom` args"

member :: Value -> Value -> EvalMonad Value
member (List l) e = return $ if e `elem` l then BoolLiteral True else BoolLiteral False
member p e = lift $ throwE $ IncorrectArgsTypes [p, e] "in `member` args"

insert :: Value -> Value -> Value -> EvalMonad Value
insert (List (List [k, _] : ms)) k1 v | k1 == k = return $ List $ List [k, v] : ms
insert (List []) k v = return $ List [List [k, v]]
insert (List (p : ms)) k v = do
  mms <- insert (List ms) k v
  cons p mms
insert p k v = lift $ throwE $ IncorrectArgsTypes [p, k, v] "in `insert` args"

lookup :: Value -> Value -> EvalMonad Value
lookup (List l) k = do
  ml <- mapM valueToPair l
  return $ fromMaybe Unit (L.lookup k ml)
lookup p k = lift $ throwE $ IncorrectArgsTypes [p, k] "in `lookup` args"

or :: Value -> Value -> EvalMonad Value
or (BoolLiteral x) (BoolLiteral y) = return $ BoolLiteral (x || y)
or x y = lift $ throwE $ IncorrectArgsTypes [x, y] "in `or` args"

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
    jumpToCommand (Goto (Label lab)) = listv [sv "goto", sv lab]
    jumpToCommand (If e (Label l1) (Label l2)) = listv [sv "if", ev e, sv l1, sv l2]
    jumpToCommand (Return c) = listv [sv "return", ev c]
commands x y = lift $ throwE $ IncorrectArgsTypes [x, y] "in `commands`"

dynamicLabels :: Value -> Value -> EvalMonad Value
dynamicLabels (Prog (Program _ bbs)) staticVars = do
  v <- valueToVarNames staticVars
  return $ List $ (\(Label l) -> StringLiteral l) <$> map head ((group . sort) (Label "init" : getLabels v bbs))
  where
    getLabels :: [VarName] -> [BasicBlock] -> [Label]
    getLabels staticVarNames =
      (=<<)
        ( \case
            (BasicBlock _ _ (If c l1 l2)) -> if exprIsStatic c staticVarNames then [] else [l1, l2] -- Here should be filtration for ifs with dynamic condition
            _ -> []
        )
dynamicLabels x y = lift $ throwE $ IncorrectArgsTypes [x, y] "in `labels`"

toLabel :: Value -> EvalMonad Value
toLabel v = return $ StringLiteral $ toLabelInternal v

toLabelInternal :: Value -> String
toLabelInternal = show

descrToProg :: Value -> Value -> Value -> EvalMonad Value
descrToProg (Prog (Program allVars _)) staticVars descr = do
  bbs <- commandToBlocks descr
  stVars <- toVars staticVars
  if not (stVars `isSubsequenceOf` allVars)
    then
      lift $ throwE $ InvalidStaticVars stVars
    else
      return $ Prog (Program (allVars \\ stVars) bbs)
  where
    toVars :: Value -> EvalMonad [VarName]
    toVars (List l) = mapM go l
      where
        go :: Value -> EvalMonad VarName
        go (List [StringLiteral name, _]) = return $ VarName name
        go x = lift $ throwE $ IncorrectArgsTypes [x] "in `toVars`"
    toVars x = lift $ throwE $ IncorrectArgsTypes [x] "in `toVars`"

    commandToBlocks :: Value -> EvalMonad [BasicBlock]
    commandToBlocks (List l) = reverse <$> mapM commandToBlock l
    commandToBlocks x = lift $ throwE $ IncorrectArgsTypes [x] "in `commandToBlocks`"

    commandToBlock :: Value -> EvalMonad BasicBlock
    commandToBlock (List l) = do
      jump <- commandToJump $ head l
      assn <- reverse <$> mapM commandToAssign (init $ tail l)
      return $ BasicBlock (Label $ toLabelInternal $ last l) assn jump
    commandToBlock x = lift $ throwE $ IncorrectArgsTypes [x] "in `commandToBlock`"

    commandToJump :: Value -> EvalMonad Jump
    commandToJump (List [StringLiteral "return", Expr e]) = return $ Return e
    commandToJump (List [StringLiteral "goto", StringLiteral l]) = return $ Goto (Label l)
    commandToJump (List [StringLiteral "if", Expr c, StringLiteral l1, StringLiteral l2]) =
      return $ If c (Label l1) (Label l2)
    commandToJump x = lift $ throwE $ IncorrectArgsTypes [x] "in `commandToJump`"

    commandToAssign :: Value -> EvalMonad Assignment
    commandToAssign (List [StringLiteral "assign", StringLiteral name, Expr e]) =
      return $ Assignment (VarName name) e
    commandToAssign x = lift $ throwE $ IncorrectArgsTypes [x] "in `commandToAssign`"
descrToProg x y z = lift $ throwE $ IncorrectArgsTypes [x, y, z] "in `commandToProgram`"

compressLabels :: Value -> Value -> EvalMonad Value
compressLabels (StringLiteral s) (Prog p) = return $ Prog $ evalState (compressLabelsProg p (Label s)) (0, M.empty)
  where
    compressLabelsProg :: Program -> Label -> State (Int, M.HashMap Label Label) Program
    compressLabelsProg (Program x bbs) initLabel = do
      modify (second (M.insert initLabel (Label "init")))
      Program x <$> mapM compressLabelsBb bbs

    compressLabelsBb :: BasicBlock -> State (Int, M.HashMap Label Label) BasicBlock
    compressLabelsBb (BasicBlock l assing j) = do
      nl <- compressLabel l
      nj <- case j of
        Goto l1 -> Goto <$> compressLabel l1
        If cond l1 l2 -> If cond <$> compressLabel l1 <*> compressLabel l2
        x -> return x
      return $ BasicBlock nl assing nj

    compressLabel :: Label -> State (Int, M.HashMap Label Label) Label
    compressLabel initialL = do
      (counter, m) <- get
      case M.lookup initialL m of
        Just v -> return v
        Nothing -> do
          let compressedLab = Label $ "l" ++ show counter
          put (counter + 1, M.insert initialL compressedLab m)
          return compressedLab
compressLabels x y = lift $ throwE $ IncorrectArgsTypes [x, y] "in `compressLabels`"

isStatic :: Value -> Value -> EvalMonad Value
isStatic (Expr e) staticVarNames = BoolLiteral . exprIsStatic e <$> valueToVarNames staticVarNames
isStatic x y = lift $ throwE $ IncorrectArgsTypes [x, y] "in `isStatic` args"

valueToVarNames :: Value -> EvalMonad [VarName]
valueToVarNames (List names) = mapM valueToVarName names
  where
    valueToVarName :: Value -> EvalMonad VarName
    valueToVarName (StringLiteral n) = return $ VarName n
    valueToVarName x = lift $ throwE $ IncorrectArgsTypes [x] "in `valueToVarName` args"
valueToVarNames x = lift $ throwE $ IncorrectArgsTypes [x] "in `valueToVarNames` args"

progLiveVariables :: Value -> EvalMonad Value
progLiveVariables (Prog p) =
  return $
    List $
      map (\(Label l, v) -> List [StringLiteral l, v]) $
        M.toList $
          M.map (List . map (\(VarName vName) -> StringLiteral vName)) (liveVariables p)
progLiveVariables x = lift $ throwE $ IncorrectArgsTypes [x] "in `liveVariables` args"

filterKeys :: Value -> Value -> EvalMonad Value
filterKeys (List mapEntries) (List l2) = do
  entries <- mapM valueToPair mapEntries
  return $ List $ L.map (\(a, b) -> List [a, b]) $ L.filter (\(a, _) -> a `elem` l2) entries
filterKeys x y = lift $ throwE $ IncorrectArgsTypes [x, y] "in `intersect` args"

valueToPair :: Value -> EvalMonad (Value, Value)
valueToPair (List [a, b]) = return (a, b)
valueToPair x = lift $ throwE $ IncorrectArgsTypes [x] "in `valueToPair` args"
