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
  | List [Value]
  | Unit
  deriving (Eq, Show)

newtype VarName = VarName String deriving (Eq, Hashable, Show, Ord)

data BasicBlock = BasicBlock Label [Assignment] Jump deriving (Eq, Show)

data Assignment = Assignment VarName Expr deriving (Eq, Show)

data Expr
  = Constant Value
  | Var VarName
  | Plus Expr Expr
  | Eq Expr Expr
  | Hd Expr
  | Tl Expr
  | Cons Expr Expr
  | SuffixFrom Expr Expr
  | Insert Expr Expr Expr
  | Lookup Expr Expr
  | Member Expr Expr
  | Commands Expr Expr
  | Eval Expr Expr
  | Reduce Expr Expr
  | IsStatic Expr Expr
  | TraceExpr Expr Expr
  | DescrToProg Expr Expr Expr
  | ToLabel Expr
  | CompressLabels Expr Expr
  | Or Expr Expr
  | DynamicLabels Expr Expr
  | FilterKeys Expr Expr
  | ProgLiveVariables Expr
  deriving (Eq, Show)

instance IsString Expr where
  fromString :: String -> Expr
  fromString = Var . VarName

data Jump = Goto Label | If Expr Label Label | Return Expr deriving (Eq, Show)

newtype Label = Label String deriving (Eq, Hashable, Show, Ord)
