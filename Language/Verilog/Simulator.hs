module Language.Verilog.Simulator
  ( Simulator
  , SimCommand  (..)
  , SimResponse (..)
  , simulator
  ) where

import Data.Array.IO
import Data.Bits
import Data.IORef
import Data.Monoid
import System.IO

import Data.VCD hiding (Var, step)
import qualified Data.VCD as VCD

import Data.BitVec
import Language.Verilog.Netlist

check msg = putStrLn msg >> hFlush stdout

-- | A Simulator executes 'SimCommand's.
type Simulator = SimCommand -> IO (Maybe SimResponse)

-- | Simulation commands.
data SimCommand
  = Init         (Maybe FilePath)  -- ^ Initalize
  | Step
  | GetSignalId  Path
  | GetSignal    NetId
  | GetInputId   Path
  | SetInput     NetId BitVec  -- ^ All output-to-input paths must be sequential.  Inputs are registered at the begining of the next step.
  | Close

-- | Simulation responses.
data SimResponse
  = Id    NetId  -- ^ Response to GetSignalId and GetInputId.
  | Value BitVec -- ^ Response to GetSignal.

-- | Builds a 'Simulator' given a 'Netlist'.
simulator :: Netlist -> IO Simulator
simulator netlist' = do
  let netlist = sortTopo netlist'
  vcd         <- newIORef Nothing
  sample      <- newIORef $ return ()
  memory      <- memory netlist
  step        <- step netlist memory sample
  return $ \ cmd -> case cmd of
    Init        file     -> initialize netlist memory vcd file sample
    Step                 -> step >> return Nothing
    GetSignalId path     -> return $ getSignalId netlist path
    GetSignal   id       -> readArray memory id        >>= return . Just . Value
    GetInputId  path     -> return $ getInputId netlist path
    SetInput    id value -> writeArray memory id value >>  return Nothing
    Close                -> close vcd sample >>  return Nothing

getSignalId :: Netlist -> Path -> Maybe SimResponse
getSignalId netlist path = case lookup path paths' of
  Nothing -> Nothing
  Just i  -> Just $ Id i
  where
  paths = [ (paths, id) | Reg id _ paths _ <- netlist ] ++ [ (paths, id) | Var id _ paths _ <- netlist ] 
  paths' = [ (path, id) | (paths, id) <- paths, path <- paths ]

getInputId :: Netlist -> Path -> Maybe SimResponse
getInputId netlist path = case lookup path paths' of
  Nothing -> Nothing
  Just i  -> Just $ Id i
  where
  paths = [ (paths, id) | Reg _ _ paths id <- netlist ]
  paths' = [ (path, id) | (paths, id) <- paths, path <- paths ]

type Memory = IOArray Int BitVec

memory :: Netlist -> IO Memory
memory netlist
  | null netlist = error "Empty netlist, nothing to simulate."
  | otherwise    = newArray (0, maximum ids) 0
  where
  ids = map f netlist
  f a = case a of
    Var a _ _ _ -> a
    Reg a _ _ _ -> a

initialize :: Netlist -> Memory -> IORef (Maybe VCDHandle) -> Maybe FilePath -> IORef (IO ()) -> IO (Maybe SimResponse)
initialize netlist memory vcd file sample = do
  close vcd sample
  mapM_ (initializeNet memory) netlist 
  case file of
    Nothing -> return ()
    Just file -> do
      h <- openFile file WriteMode
      vcd' <- newVCD h S
      writeIORef vcd $ Just vcd'
      writeIORef sample $ VCD.step vcd' 1
      mapM_ (f memory vcd' sample) netlist
  return Nothing
  where
  f :: Memory -> VCDHandle -> IORef (IO ()) -> Net -> IO ()
  f memory vcd sample a = mapM_ (\ signal -> do
    sample' <- var vcd signal $ bitVec width 0
    modifyIORef sample (>> (readArray memory i >>= sample'))
    ) signals
    where
    (i, width, signals) = case a of
      Reg i w p _ -> (i, w, p)
      Var i w p _ -> (i, w, p)

initializeNet :: Memory -> Net -> IO ()
initializeNet memory a = case a of
  Var i w _ _ -> writeArray memory i $ bitVec w 0
  Reg i w _ _ -> writeArray memory i $ bitVec w 0

close :: IORef (Maybe VCDHandle) -> IORef (IO ()) -> IO ()
close vcd sample = do
  vcd' <- readIORef vcd
  case vcd' of
    Nothing -> return ()
    Just vcd -> hClose $ handle vcd
  writeIORef vcd $ Nothing
  writeIORef sample $ return ()

step :: Netlist -> Memory -> IORef (IO ()) -> IO (IO ())
step netlist memory sample = do
  let steps = map stepNet netlist
  return $ do
    sequence_ steps
    readIORef sample >>= id
  where
  read  = readArray memory
  write' = writeArray memory
  stepNet :: Net -> IO ()
  stepNet a = case a of
    Reg q _ _ d -> read d >>= write' q
    Var i _ _ expr -> case expr of
      AInput        -> return ()
      AVar    a     -> read a >>= write
      AConst  a     -> write a
      ASelect a b c -> do { a <- read a; b <- read b; c <- read c; write $ select a (b, c) }
      ABWNot  a     -> read a >>= write . complement
      ABWAnd  a b   -> do { a <- read a; b <- read b; write $ a .&. b }
      ABWXor  a b   -> do { a <- read a; b <- read b; write $ a `xor` b }
      ABWOr   a b   -> do { a <- read a; b <- read b; write $ a .|. b }
      AMul    a b   -> do { a <- read a; b <- read b; write $ a * b }
      AAdd    a b   -> do { a <- read a; b <- read b; write $ a + b }
      ASub    a b   -> do { a <- read a; b <- read b; write $ a - b }
      AShiftL a b   -> do { a <- read a; b <- read b; write $ shiftL a $ fromIntegral $ value b }
      AShiftR a b   -> do { a <- read a; b <- read b; write $ shiftR a $ fromIntegral $ value b }
      AEq     a b   -> do { a <- read a; b <- read b; write $ bitVec 1 (if value a == value b then 1 else 0) }
      ANe     a b   -> do { a <- read a; b <- read b; write $ bitVec 1 (if value a /= value b then 1 else 0) }
      ALt     a b   -> do { a <- read a; b <- read b; write $ bitVec 1 (if value a <  value b then 1 else 0) }
      ALe     a b   -> do { a <- read a; b <- read b; write $ bitVec 1 (if value a <= value b then 1 else 0) }
      AGt     a b   -> do { a <- read a; b <- read b; write $ bitVec 1 (if value a >  value b then 1 else 0) }
      AGe     a b   -> do { a <- read a; b <- read b; write $ bitVec 1 (if value a >= value b then 1 else 0) }
      AMux    a b c -> do { a <- read a; b <- read b; c <- read c; write (if value a /= 0 then b else c) }
      AConcat a b   -> do { a <- read a; b <- read b; write $ mappend a b }
      where
      write = write' i

