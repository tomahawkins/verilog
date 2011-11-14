module Language.Verilog.Types
  ( Identifier
  , Module     (..)
  , ModuleItem (..)
  , Stmt       (..)
  , Expr       (..)
  , Sense      (..)
  , LHS        (..)
  , Case
  , Range
  ) where

type Identifier = String

data Module = Module Identifier [Identifier] [ModuleItem] deriving (Show, Eq)

{-
instance Show Module where
  show (Module name ports items) = unlines
    [ "module " ++ name ++ (if null ports then "" else "(" ++ intercalate ", " ports ++ ")") ++ ";"
    , "endmodule"
    ]
-}

data ModuleItem
  = Paremeter Identifier Expr
  | Input     (Maybe Range) [(Identifier, Maybe Range)]
  | Output    (Maybe Range) [(Identifier, Maybe Range)]
  | Inout     (Maybe Range) [(Identifier, Maybe Range)]
  | Wire      (Maybe Range) [(Identifier, Maybe Range)]
  | Reg       (Maybe Range) [(Identifier, Maybe Range)]
  | Initial    Stmt
  | Always     Sense Stmt
  | Assign     LHS Expr
  deriving (Show, Eq)

data Expr
  = String     String
  | Number     String
  | ExprLHS    LHS
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
  = Block                 (Maybe Identifier) [Stmt]
  | Integer               Identifier
  | Case                  Expr [Case] (Maybe Stmt)
  | BlockingAssignment    LHS Expr
  | NonBlockingAssignment LHS Expr
  | For                   (Identifier, Expr) Expr (Identifier, Expr) Stmt
  | If                    Expr Stmt Stmt
  | Call                  Identifier [Expr]
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
  = LHS      Identifier
  | LHSBit   Identifier Expr
  | LHSRange Identifier Range
  deriving (Show, Eq)

type Range = (Expr, Expr)

