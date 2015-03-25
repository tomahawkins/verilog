module Main (main) where

import System.Environment

import Language.Verilog

import Inline

main :: IO ()
main = do
  args <- getArgs
  modules <- mapM (\ f -> readFile f >>= return . parseFile [] f) args >>= return . concat
  let m = inline modules
  print m

