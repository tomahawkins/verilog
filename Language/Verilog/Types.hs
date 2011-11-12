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
  deriving (Show, Eq)

data Expr
  = Expr
  deriving (Show, Eq)

