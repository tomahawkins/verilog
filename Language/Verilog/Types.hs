module Language.Verilog.Types
  ( Name
  , Module     (..)
  , ModuleItem (..)
  , Expr       (..)
  ) where

type Name = String

data Module = Module Name [ModuleItem] deriving (Show, Eq)

data ModuleItem
  = Paremeter Name Expr
  | Input     (Maybe (Expr, Expr)) [Name]
  | Output    (Maybe (Expr, Expr)) [Name]
  | Inout     (Maybe (Expr, Expr)) [Name]
  | Wire      (Maybe (Expr, Expr)) [Name]
  | Reg       (Maybe (Expr, Expr)) [Name]
  deriving (Show, Eq)

data Expr
  = String     String
  | Number     String
  | Identifier String
  deriving (Show, Eq)

