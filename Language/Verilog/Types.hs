module Language.Verilog.Types
  ( Name
  , Module (..)
  ) where

type Name = String

data Module = Module Name deriving (Show, Eq)

