module Language.Verilog.Preprocess
  ( uncomment
  ) where

-- | Remove comments from code.
uncomment :: FilePath -> String -> String
uncomment file a = uncomment a
  where
  uncomment a = case a of
    ""               -> ""
    '/' : '/' : rest -> "  " ++ removeEOL rest
    '/' : '*' : rest -> "  " ++ remove rest
    '"'       : rest -> '"' : ignoreString rest
    a         : rest -> a   : uncomment rest

  removeEOL a = case a of
    ""          -> ""
    '\n' : rest -> '\n' : uncomment rest 
    '\t' : rest -> '\t' : removeEOL rest
    _    : rest -> ' '  : removeEOL rest

  remove a = case a of
    ""               -> error $ "File ended without closing comment (*/): " ++ file
    '"' : rest       -> removeString rest
    '\n' : rest      -> '\n' : remove rest
    '\t' : rest      -> '\t' : remove rest
    '*' : '/' : rest -> "  " ++ uncomment rest
    _ : rest         -> " "  ++ remove rest

  removeString a = case a of
    ""                -> error $ "File ended without closing string: " ++ file
    '"' : rest        -> " "  ++ remove       rest
    '\\' : '"' : rest -> "  " ++ removeString rest
    '\n' : rest       -> '\n' :  removeString rest
    '\t' : rest       -> '\t' :  removeString rest
    _    : rest       -> ' '  :  removeString rest

  ignoreString a = case a of
    ""                -> error $ "File ended without closing string: " ++ file
    '"' : rest        -> '"' : uncomment rest
    '\\' : '"' : rest -> "\\\"" ++ ignoreString rest
    a : rest          -> a : ignoreString rest

{-

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
    -}
