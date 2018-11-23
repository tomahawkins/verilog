{-# LANGUAGE OverloadedStrings #-}

module Language.Verilog.AST
  ( Identifier
  , Module     (..)
  , ModuleItem (..)
  , Stmt       (..)
  , LHS        (..)
  , Expr       (..)
  , UniOp      (..)
  , BinOp      (..)
  , Sense      (..)
  , Call       (..)
  , PortBinding
  , Case
  , Range
  ) where

import Data.Bits
import Data.List
import Data.Maybe
import Data.Semigroup
import Data.Text hiding (null, intercalate)

import Data.BitVec

type Identifier = String

data Module = Module Identifier [Identifier] [ModuleItem]
  deriving (Eq, Show)

data ModuleItem
  = Comment    String
  | Parameter  (Maybe Range) Identifier Expr
  | Localparam (Maybe Range) Identifier Expr
  | Input      (Maybe Range) [Identifier]
  | Output     (Maybe Range) [Identifier]
  | Inout      (Maybe Range) [Identifier]
  | Wire       (Maybe Range) [(Identifier, Maybe Expr)]
  | Reg        (Maybe Range) [(Identifier, Maybe Range)]
  | Integer    [Identifier]
  | Initial    Stmt
  | Always     (Maybe Sense) Stmt
  | Assign     LHS Expr
  | Instance   Identifier [PortBinding] Identifier [PortBinding]
  deriving (Eq, Show)

type PortBinding = (Identifier, Maybe Expr)

data Expr
  = String     String
  | Number     BitVec
  | ConstBool  Bool
  | Ident      Identifier
  | IdentRange Identifier Range
  | IdentBit   Identifier Expr
  | Repeat     Expr [Expr]
  | Concat     [Expr]
  | ExprCall   Call
  | UniOp      UniOp Expr
  | BinOp      BinOp Expr Expr
  | Mux        Expr Expr Expr
  | Bit        Expr Int
  deriving (Eq, Show)

data UniOp = Not | BWNot | UAdd | USub
  deriving (Eq, Show)

data BinOp
  = And   
  | Or    
  | BWAnd 
  | BWXor 
  | BWOr  
  | Mul   
  | Div   
  | Mod   
  | Add   
  | Sub   
  | ShiftL
  | ShiftR
  | Eq    
  | Ne    
  | Lt    
  | Le    
  | Gt    
  | Ge    
  deriving (Eq, Show)

instance Num Expr where
  (+) = BinOp Add
  (-) = BinOp Sub
  (*) = BinOp Mul
  negate = UniOp USub
  abs = undefined
  signum = undefined
  fromInteger = Number . fromInteger

instance Bits Expr where
  (.&.) = BinOp BWAnd
  (.|.) = BinOp BWOr
  xor   = BinOp BWXor
  complement = UniOp BWNot
  isSigned _ = False
  shift        = error "Not supported: shift"
  rotate       = error "Not supported: rotate"
  bitSize      = error "Not supported: bitSize"
  bitSizeMaybe = error "Not supported: bitSizeMaybe"
  testBit      = error "Not supported: testBit"
  bit          = error "Not supported: bit"
  popCount     = error "Not supported: popCount"

instance Monoid Expr where
  mempty      = 0
  mappend a b = mconcat [a, b]
  mconcat     = Concat

data LHS
  = LHS       Identifier
  | LHSBit    Identifier Expr
  | LHSRange  Identifier Range
  | LHSConcat [LHS]
  deriving (Eq, Show)

data Stmt
  = Block                 (Maybe Identifier) [Stmt]
  | StmtReg               (Maybe Range) [(Identifier, Maybe Range)]
  | StmtInteger           [Identifier]
  | Case                  Expr [Case] (Maybe Stmt)
  | BlockingAssignment    LHS Expr
  | NonBlockingAssignment LHS Expr
  | For                   (Identifier, Expr) Expr (Identifier, Expr) Stmt
  | If                    Expr Stmt Stmt
  | StmtCall              Call
  | Delay                 Expr Stmt
  | Null
  deriving (Eq, Show)

type Case = ([Expr], Stmt)

data Call = Call Identifier [Expr]
  deriving (Eq, Show)

data Sense
  = Sense        LHS
  | SenseOr      Sense Sense
  | SensePosedge LHS
  | SenseNegedge LHS
  deriving (Eq, Show)

type Range = (Expr, Expr)

