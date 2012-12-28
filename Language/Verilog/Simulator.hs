module Language.Verilog.Simulator
  ( simulate
  , Assignment (..)
  , Path
  , AExpr (..)
  ) where

import Language.Verilog.AST
import Language.Verilog.Simulator.ANF
import Language.Verilog.Simulator.Compile

-- | Simulation given the top level module name, a list of modules, and a test bench function.
simulate :: [Module] -> Identifier -> Identifier -> IO ()
simulate modules top _ = do
  _ <- compile modules top
  return ()

