-- | An embedded DSL for Verilog.
module Language.Verilog.DSL
  ( Verilog
  , elaborate
  , getMeta
  , setMeta
  , genVar
  -- * Signal Declarations
  , input
  , output
  , wire
  , reg
  -- * Module Statements
  , assign
  , next
  -- * Expressions
  , var
  , constant
  , mappend
  , mconcat
  , (<>)
  , mux
  ) where

import Data.Monoid
import MonadLib

import Data.BitVec
import Language.Verilog.AST

type Verilog a = StateT (Int, a, [ModuleItem]) Id

elaborate :: String -> a -> Verilog a () -> (a, Module)
elaborate name a m = (a', Module name (concatMap port items) items)
  where
  ((), (_, a', items)) = runId $ runStateT (0, a, []) m

  port :: ModuleItem -> [String]
  port a = case a of
    Input  _ a -> a
    Output _ a -> a
    Inout  _ a -> a
    _ -> []

getMeta :: Verilog a a
getMeta = do
  (_, a, _) <- get
  return a

setMeta :: a -> Verilog a ()
setMeta a = do
  (i, _, m) <- get
  set (i, a, m)

genVar :: Verilog a String
genVar = do
  (i, a, m) <- get
  set (i + 1, a, m)
  return $ "_verilog_" ++ show i

item :: ModuleItem -> Verilog a ()
item b = do
  (i, a, m) <- get
  set (i, a, m ++ [b])

input :: String -> Int -> Verilog a ()
input a w = item $ Input (range w) [a]

output :: String -> Int -> Verilog a ()
output a w = item $ Output (range w) [a]

wire :: String -> Int -> Maybe Expr -> Verilog a ()
wire a w b = item $ Wire (range w) [(a, b)]

reg :: String -> Int -> Verilog a ()
reg a w = item $ Reg (range w) [(a, Nothing)]

range :: Int -> Maybe Range
range w
  | w == 1    = Nothing
  | otherwise = Just (fromIntegral $ w - 1, 0)

assign :: String -> Expr -> Verilog a ()
assign a b = item $ Assign (LHS a) b

-- | Next state of a reg.  An abstraction around always.  Assume "clock" and "reset" are signals.
next :: String -> Expr -> Verilog a ()
next a b = item $ Always (SensePosedge $ LHS "clock") $ If (ExprLHS $ LHS "reset") (NonBlockingAssignment (LHS a) 0) (NonBlockingAssignment (LHS a) b) 

var :: String -> Expr
var = ExprLHS . LHS

constant :: Int -> Integer -> Expr
constant a b = Number $ bitVec a b

mux :: Expr -> Expr -> Expr -> Expr
mux = Mux
