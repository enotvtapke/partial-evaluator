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

import Text.Printf (printf)
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
  deriving (Eq)

instance Show Value where
  show :: Value -> String
  show (IntLiteral i) = show i
  show (BoolLiteral b) = show b
  show (StringLiteral s) = show s
  show (List l) = printf "%s" (show l)
  show Unit = "()"
  show (Expr e) = printf "`%s`" (show e)
  show (Prog p) = printf "p`%s`" (show p)

newtype VarName = VarName String deriving (Eq, Hashable, Show)

data BasicBlock = BasicBlock { label :: Label, assigns :: [Assignment], jmp :: Jump } deriving (Eq, Show)

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
  | DynamicLabels Expr
  deriving (Eq, Show)

instance IsString Expr where
  fromString :: String -> Expr
  fromString = Var . VarName

data Jump = Goto Label | If Expr Label Label | Return Expr deriving (Eq, Show)

newtype Label = Label String deriving (Eq, Hashable, Show)
