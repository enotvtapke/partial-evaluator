{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Flowchart.AST
  ( Program (..),
    Value (..),
    VarName (..),
    BasicBlock (..),
    Assignment (..),
    Expr (..),
    Jump (..),
    Label (..),
  )
where

import Data.Hashable
import Data.String (IsString(..))
data Program = Program [VarName] [BasicBlock] deriving (Eq, Show)

data Value
  = IntLiteral Int
  | Expr Expr
  | Prog Program
  | BoolLiteral Bool
  | StringLiteral String
  | Pair Value Value
  | Unit
  deriving (Eq, Show)

newtype VarName = VarName String deriving (Eq, Hashable, Show)

data BasicBlock = BasicBlock { label :: Label, assigns :: [Assignment], jmp :: Jump } deriving (Eq, Show)

data Assignment = Assignment VarName Expr deriving (Eq, Show)

data Expr
  = Constant Value
  | Var VarName
  | Plus Expr Expr
  | Eq Expr Expr
  | Car Expr
  | Cdr Expr
  | Cons Expr Expr
  | SuffixFrom Expr Expr
  | Insert Expr Expr Expr
  | Lookup Expr Expr  -- Lookup in map on pairs
  | Member Expr Expr -- Searching in list
  | Commands Expr Expr -- get list of commands in basic block. Takes program and label
  | Eval Expr Expr
  | Reduce Expr Expr
  deriving (Eq, Show)

instance IsString Expr where
  fromString :: String -> Expr
  fromString = Var . VarName

data Jump = Goto Label | If Expr Label Label | Return Expr deriving (Eq, Show)

newtype Label = Label String deriving (Eq, Hashable, Show)
