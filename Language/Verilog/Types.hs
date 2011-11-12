module Language.Verilog.Types
  ( Name
  , Range
  , Module     (..)
  , ModuleItem (..)
  , Expr       (..)
  ) where

type Name = String

type Range = (Expr, Expr)

data Module = Module Name [ModuleItem] deriving (Show, Eq)

data ModuleItem
  = Paremeter Name Expr
  | Input     (Maybe Range) [(Name, Maybe Range)]
  | Output    (Maybe Range) [(Name, Maybe Range)]
  | Inout     (Maybe Range) [(Name, Maybe Range)]
  | Wire      (Maybe Range) [(Name, Maybe Range)]
  | Reg       (Maybe Range) [(Name, Maybe Range)]
  deriving (Show, Eq)

data Expr
  = String     String
  | Number     String
  | Identifier String
  | Not        Expr
  | And        Expr Expr
  | Mul        Expr Expr
  | Div        Expr Expr
  | Mod        Expr Expr
  | Add        Expr Expr
  | Sub        Expr Expr
  deriving (Show, Eq)

