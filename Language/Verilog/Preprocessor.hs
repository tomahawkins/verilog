module Language.Verilog.Preprocessor
  ( preprocess
  ) where

preprocess :: String -> [(String, Int, String)] -- File name, starting line, and data.
preprocess s = deline' "unknown" 1 (lines s)
  where

  deline' :: String -> Int -> [String] -> [(String,Int,String)]
  deline' _ _ []    = []
  deline' f l lines = let (code, next) = span isCode lines in
                      (f, l, unlines code) : deline'' next
  
  deline'' :: [String] -> [(String,Int,String)]
  deline'' []     = []
  deline'' (l:ls) = case words l of
    ["`line",line,file,_] -> deline' (tail (init file)) (read line) ls
    _                     -> error ("Bad `line format: " ++ l)
                      
  isCode :: String -> Bool
  isCode line = case words line of
    ["`line",_,_,_] -> False
    _               -> True
