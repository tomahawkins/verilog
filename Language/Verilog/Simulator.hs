module Language.Verilog.Simulator
  ( Simulator
  , SimCommand  (..)
  , SimResponse (..)
  , simulator
  ) where

import Data.Array.IO
import Data.IORef
import System.IO

import Data.VCD hiding (Var)

import Data.BitVec
import Language.Verilog.AST (Module, Identifier)
import Language.Verilog.Simulator.ANF
import Language.Verilog.Simulator.ANormalize

-- | A Simulator executes 'SimCommand's.
type Simulator = SimCommand -> IO (Maybe SimResponse)

-- | Simulation commands.
data SimCommand
  = Init         (Maybe FilePath)  -- ^ Initalize
  | Step
  | GetSignalId  Path
  | GetSignal    Int 
  | GetInputId   Path
  | SetInput     Int BitVec  -- ^ All output-to-input paths must be sequential.  Inputs are registered at the begining of the next step.
  | Close

-- | Simulation responses.
data SimResponse
  = Id    Int    -- ^ Response to GetSignalId and GetInputId.
  | Value BitVec -- ^ Response to GetSignal.

-- | Builds a 'Simulator' given a list of modules and the top level module name.
simulator :: [Module] -> Identifier -> IO Simulator
simulator modules top = do
  anf         <- aNormalize modules top >>= return . sortTopo
  initialized <- newIORef False
  vcd         <- newIORef Nothing
  memory      <- memory anf
  getSignalId <- error "getSignalId"
  getInputId  <- error "getInputId"
  step        <- error "step"
  return $ \ cmd -> case cmd of
    Init        file     -> initialize anf initialized memory vcd file >> return Nothing
    Step                 -> step                       >>  return Nothing
    GetSignalId path     -> getSignalId path           >>= return . Just . Id
    GetSignal   id       -> readArray memory id        >>= return . Just . Value
    GetInputId  path     -> getInputId path            >>= return . Just . Id
    SetInput    id value -> writeArray memory id value >>  return Nothing
    Close                -> close initialized vcd      >>  return Nothing

type Memory = IOArray Int BitVec

memory :: NetList -> IO Memory
memory anf = newArray (0, maximum ids) 0
  where
  ids = map f anf
  f a = case a of
    Var a _ _ _ -> a
    Reg a _ _ _ -> a

initialize :: NetList -> IORef Bool -> Memory -> IORef (Maybe VCDHandle) -> Maybe FilePath -> IO ()
initialize anf initialized memory vcd file = do
  close initialized vcd
  writeIORef initialized True
  mapM_ (initializeNet memory) anf
  case file of
    Nothing -> writeIORef vcd Nothing
    Just file -> do
      h <- openFile file WriteMode
      vcd' <- newVCD h S
      --XXX Define signals.
      writeIORef vcd $ Just vcd'

initializeNet :: Memory -> Net -> IO ()
initializeNet memory a = case a of
  Var i w _ _ -> writeArray memory i $ bitVec w 0
  Reg i w _ _ -> writeArray memory i $ bitVec w 0

close :: IORef Bool -> IORef (Maybe VCDHandle) -> IO ()
close initialized vcd = do
  writeIORef initialized False
  vcd' <- readIORef vcd
  case vcd' of
    Nothing -> return ()
    Just vcd -> hClose $ handle vcd

{-
run :: Int -> FilePath -> [Assignment] -> IO ()
run cycles vcd assignments = do
  h <- openFile vcd WriteMode
  vcd <- newVCD h S
  steps <- mapM (prep vcd) assignments
  replicateM_ cycles $ sequence_ $ step vcd 1 : steps
  hClose h

--check :: String -> IO ()
--check msg = putStrLn msg >> hFlush stdout

prep :: VCDHandle -> Assignment -> IO (IO ())
prep vcd a = case a of
  AssignVar (Var v w signals) a -> do
    samplers <- mapM (\ signal -> var vcd signal $ bitVec w 0) signals
    let sample a = sequence_ $ map ($ a) samplers
    return $ sample $ bitVec w 22 --XXX
  --AssignReg Var Var AExpr
  AssignReg _ _ _ -> return $ return ()  --XXX
-}
