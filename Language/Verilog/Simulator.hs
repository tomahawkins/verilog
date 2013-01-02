module Language.Verilog.Simulator
  ( simulate
  , Assignment (..)
  , Path
  , AExpr (..)
  ) where

import Language.Verilog.AST
import Language.Verilog.Simulator.ANF
import Language.Verilog.Simulator.Compile
import Language.Verilog.Simulator.Run

-- | Simulate a design given a list of modules, the top level module name, and the vcd file name.
simulate :: [Module] -> Identifier -> Int -> FilePath -> IO ()
simulate modules top cycles vcd = compile modules top >>= run cycles vcd

