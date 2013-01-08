module Language.Verilog.Netlist
  ( Path
  , NetId
  , Width
  , Net     (..)
  , Netlist
  , AExpr   (..)
  , sortTopo
  ) where

import Data.List

import Language.Verilog.AST (Identifier)

-- | A Path is a hierarchical path of identifiers.
type Path = [Identifier]

-- | Signal identifier.
type NetId = Int

type Width  = Int

-- | A sequence of variable assignments and memory updates in A-normal form.
data Net
  = Var NetId Width [Path] AExpr  -- ^ Signal, width, paths, signal expression.
  | Reg NetId Width [Path] NetId  -- ^ Signal, width, paths, associated D-inputs.
  deriving Show

type Netlist = [Net]

data AExpr
  = AInput
  | AVar    NetId
  | AConst  Int Integer    -- ^ Width, value.
  | ASelect NetId Int Int  -- ^ LSB is 0.
  | ABWNot  NetId
  | ABWAnd  NetId NetId
  | ABWXor  NetId NetId
  | ABWOr   NetId NetId
  | AMul    NetId NetId
  | AAdd    NetId NetId
  | ASub    NetId NetId
  | AShift  NetId Int      -- ^ Shift left if positive, right if negative.
  | AEq     NetId NetId
  | ANe     NetId NetId
  | ALt     NetId NetId
  | ALe     NetId NetId
  | AGt     NetId NetId
  | AGe     NetId NetId
  | AMux    NetId NetId NetId
  | AConcat NetId NetId
  deriving Show

sortTopo :: Netlist -> Netlist
sortTopo a = regs ++ [ Var id width paths expr | (id, width, paths, expr) <- f [] regIds vars ]
  where
  (regs, regIds) = unzip [ (r, id) | r@(Reg id _ _ _) <- a ]
  vars = [ (id, width, paths, expr) | Var id width paths expr <- a ]
  f sofar avail rest
    | null rest = sofar
    | null next = error $ "Combinational loop somewhere in : " ++ unlines (map show l1)  --XXX Not a combinational loop problem.  Variables are references, but not defined in the netlist.
    | otherwise = f (sofar ++ next) (avail ++ [ id | (id, _, _, _) <- next ]) rest'
    where
    (next, rest') = partition p rest
    p (_, _, _, expr) = all (flip elem avail) $ deps expr
    l1 = sort [ (i, deps e) | (i, _, _, e) <- rest ]

  deps :: AExpr -> [NetId]
  deps a = case a of
    AInput        -> []
    AVar    a     -> [a]
    AConst  _ _   -> []
    ASelect a _ _ -> [a]
    ABWNot  a     -> [a]
    ABWAnd  a b   -> [a, b]
    ABWXor  a b   -> [a, b]
    ABWOr   a b   -> [a, b]
    AMul    a b   -> [a, b]
    AAdd    a b   -> [a, b]
    ASub    a b   -> [a, b]
    AShift  a _   -> [a]
    AEq     a b   -> [a, b]
    ANe     a b   -> [a, b]
    ALt     a b   -> [a, b]
    ALe     a b   -> [a, b]
    AGt     a b   -> [a, b]
    AGe     a b   -> [a, b]
    AMux    a b c -> [a, b, c]
    AConcat a b   -> [a, b]

