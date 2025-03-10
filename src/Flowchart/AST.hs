{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

data Program = Program [VarName] [BasicBlock] deriving (Eq, Show)

data Value
  = IntLiteral Int
  | AST Program
  | BoolLiteral Bool
  deriving (Eq, Show)

newtype VarName = VarName String deriving (Eq, Hashable, Show)

data BasicBlock = BasicBlock Label [Assignment] Jump deriving (Eq, Show)

data Assignment = Assignment VarName Expr deriving (Eq, Show)

data Expr
  = Constant Value
  | Var VarName
  | Plus Expr Expr
  | Eq Expr Expr
  deriving (Eq, Show)

data Jump = Goto Label | If Expr Label Label | Return Expr deriving (Eq, Show)

newtype Label = Label String deriving (Eq, Hashable, Show)
