module Language.Verilog.Simulator
  ( simulate
  ) where

import Data.VCD ()

import Language.Verilog.AST

-- | Simulation the top level module name, a list of modules, and a test bench function.
simulate :: Identifier -> [Module] -> ([Integer] -> IO [Integer]) -> IO ()
simulate = undefined

{-
initialize :: Identifier -> [Module] -> 

compile :: Identifier -> [Module] -> 


data Assignment
  = AssignVar Int (Maybe [String]) Expr
  | AssignMem Int Expr Expr
-}
