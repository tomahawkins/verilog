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
import qualified Data.IntSet as S

import Data.BitVec
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
  | AConst  BitVec
  | ASelect NetId NetId NetId  -- ^ LSB is 0.
  | ABWNot  NetId
  | ABWAnd  NetId NetId
  | ABWXor  NetId NetId
  | ABWOr   NetId NetId
  | AMul    NetId NetId
  | AAdd    NetId NetId
  | ASub    NetId NetId
  | AShiftL NetId NetId
  | AShiftR NetId NetId
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
sortTopo a
  | not $ S.null unknownRegDeps = error $ "Netlist contains unknown register dependencies (D input): " ++ show (S.toList unknownRegDeps)
  | not $ S.null unknownVarDeps = error $ "Netlist contains unknown variable dependencies: "           ++ show (S.toList unknownVarDeps)
  | otherwise = regs ++ [ Var id width paths expr | (id, width, paths, expr) <- f [] regIds vars ]
  where
  f sofar avail rest
    | null rest = reverse sofar
    | null next = error $ "Combinational loop somewhere in :\n" ++ unlines (map show l1)  --XXX Not a combinational loop problem.  Variables are references, but not defined in the netlist.
    | otherwise = f (next ++ sofar) (S.union avail $ S.fromList [ id | (id, _, _, _) <- next ]) rest'
    where
    (next, rest') = partition p rest
    p (_, _, _, expr) = all (flip S.member avail) $ deps expr
    l1 = sort $ removeSinks [ (i, deps e) | (i, _, _, e) <- rest ]

  (regs, regDeps) = unzip [ (r, (q, d)) | r@(Reg q _ _ d) <- a ]
  vars = [ (id, width, paths, expr) | Var id width paths expr <- a ]

  varDeps :: [(Int, [Int])]
  varDeps = [ (i, deps e) | (i, _, _, e) <- vars ]

  regIds  = S.fromList $ fst $ unzip regDeps
  varIds  = S.fromList $ fst $ unzip varDeps
  allIds  = S.union regIds varIds

  unknownRegDeps  = S.difference (S.fromList $ snd $ unzip regDeps) allIds
  unknownVarDeps  = S.difference (S.fromList $ concat $ snd $ unzip varDeps) allIds

deps :: AExpr -> [NetId]
deps a = case a of
  AInput        -> []
  AVar    a     -> [a]
  AConst  _     -> []
  ASelect a b c -> [a, b, c]
  ABWNot  a     -> [a]
  ABWAnd  a b   -> [a, b]
  ABWXor  a b   -> [a, b]
  ABWOr   a b   -> [a, b]
  AMul    a b   -> [a, b]
  AAdd    a b   -> [a, b]
  ASub    a b   -> [a, b]
  AShiftL a b   -> [a, b]
  AShiftR a b   -> [a, b]
  AEq     a b   -> [a, b]
  ANe     a b   -> [a, b]
  ALt     a b   -> [a, b]
  ALe     a b   -> [a, b]
  AGt     a b   -> [a, b]
  AGe     a b   -> [a, b]
  AMux    a b c -> [a, b, c]
  AConcat a b   -> [a, b]

{-
findLoop :: [(Int, [Int])] -> [Int]
findLoop deps' =
  where
  deps = M.fromList deps'
  f :: [Int] -> [[Int]] -> 
  f toExplore
  -}

removeSinks :: [(Int, [Int])] -> [(Int, [Int])]
removeSinks a
  | S.null sinks = a
  | otherwise    = removeSinks [ (a, b) | (a, b) <- a, not $ S.member a sinks ]
  where
  nodes = S.fromList $ fst $ unzip a
  deps  = S.fromList $ concat $ snd $ unzip a
  sinks = S.difference nodes deps

