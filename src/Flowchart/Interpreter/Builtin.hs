module Flowchart.Interpreter.Builtin
  ( plus,
    eq,
    car,
    cdr,
    cons,
    suffixFrom,
    member,
    insert,
    lookup,
    commands,
  )
where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except (throwE)
import Data.List (find)
import Flowchart.AST
import Flowchart.DSL (ev, listv, sv)
import Flowchart.Interpreter.EvalState
import Prelude hiding (lookup)

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
