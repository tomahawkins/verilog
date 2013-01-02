module Language.Verilog.Simulator.Run
  ( run
  ) where

import Control.Monad
import System.IO

import Data.VCD hiding (Var)

import Data.BitVec
import Language.Verilog.Simulator.ANF

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
    --print w
    --hFlush stdout
    samplers <- mapM (\ signal -> var vcd signal $ bitVec w 0) signals
    --let sample a = sequence_ $ map ($ a) samplers
    --return $ sample $ bitVec w 22 --XXX
    return (return ())
  --AssignReg Var Var AExpr
  AssignReg _ _ _ -> return $ return ()  --XXX

