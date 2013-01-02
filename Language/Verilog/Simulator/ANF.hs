module Language.Verilog.Simulator.ANF
  ( Assignment (..)
  , AExpr      (..)
  , Var        (..)
  , Path
  ) where

import Language.Verilog.AST (Identifier)

-- | A Path is a hierarchical path of identifiers.
type Path = [Identifier]

-- | A sequence of variable assignments and memory updates in A-normal form.
data Assignment
  = AssignVar Var AExpr
  | AssignReg Var Var AExpr

data Var = Var Int Int [Path]  -- ^ Uid, width, path list of all signals tied together.

data AExpr
  = AConst      Int Integer  -- ^ Width, value.
  | ASelect     Var Int Int  -- ^ LSB is 0.
  | ABWNot      Var
  | ABWAnd      Var Var
  | ABWXor      Var Var
  | ABWOr       Var Var
  | AMul        Var Var
  | AAdd        Var Var
  | ASub        Var Var
  | AShiftL     Var Int
  | AShiftR     Var Int
  | AEq         Var Var
  | ANe         Var Var
  | ALt         Var Var
  | ALe         Var Var
  | AGt         Var Var
  | AGe         Var Var
  | AMux        Var Var Var
  | AConcat     [Var]

