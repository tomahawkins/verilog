module Language.Verilog.Simulator
  ( simulate
  , compile
  , Assignment (..)
  , Path
  , AExpr (..)
  ) where

import Data.VCD ()

import Language.Verilog.AST

-- | Simulation given the top level module name, a list of modules, and a test bench function.
simulate :: Identifier -> [Module] -> ([Integer] -> IO [Integer]) -> IO ()
simulate = undefined

compile :: Identifier -> [Module] -> [Assignment]
compile top modules = undefined



-- | A Path is a hierarchical path of identifiers.
type Path = [Identifier]

-- | A sequence of variable assignments and memory updates in A-normal form.
data Assignment
  = AssignVar Var AExpr
  | AssignReg Var AExpr
  | AssignMem Var AExpr AExpr

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

