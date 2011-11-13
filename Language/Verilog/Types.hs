module Language.Verilog.Types
  ( Name
  , Range
  , Module     (..)
  , ModuleItem (..)
  , Stmt       (..)
  , Expr       (..)
  , Sense      (..)
  , LHS        (..)
  , Case
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
  | Initial    Stmt
  | Always     Sense Stmt
  | Assign     LHS Expr
  deriving (Show, Eq)

data Expr
  = String     String
  | Number     String
  | Identifier String
  | Not        Expr
  | And        Expr Expr
  | Or         Expr Expr
  | BWNot      Expr
  | BWAnd      Expr Expr
  | BWXor      Expr Expr
  | BWOr       Expr Expr
  | Mul        Expr Expr
  | Div        Expr Expr
  | Mod        Expr Expr
  | Add        Expr Expr
  | Sub        Expr Expr
  | ShiftL     Expr Expr
  | ShiftR     Expr Expr
  | Eq         Expr Expr
  | Ne         Expr Expr
  | Lt         Expr Expr
  | Le         Expr Expr
  | Gt         Expr Expr
  | Ge         Expr Expr
  | Mux        Expr Expr Expr
  | Repeat     Expr [Expr]
  | Concat     [Expr]
  deriving (Show, Eq)

data Stmt
  = Block                 (Maybe Name) [Stmt]
  | Integer               Name
  | Case                  Expr [Case] Stmt
  | BlockingAssignment    LHS Expr
  | NonBlockingAssignment LHS Expr
  | For                   Stmt Expr Stmt Stmt
  | If                    Expr Stmt Stmt
  | Call                  Name [Expr]
  | Null
  deriving (Show, Eq)

type Case = ([Expr], Stmt)

data Sense
  = Sense        LHS
  | SenseOr      Sense Sense
  | SensePosedge LHS
  | SenseNegedge LHS
  deriving (Show, Eq)

data LHS
  = LHS      Name
  | LHSBit   Name Expr
  | LHSRange Name Range
  deriving (Show, Eq)

