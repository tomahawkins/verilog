{-# LANGUAGE OverloadedStrings #-}

module Language.Verilog.Parser.Preprocess where

import Data.Semigroup
import Data.Text hiding (head, tail, null)
import Prelude hiding (takeWhile, drop, length, words, unwords, lines, unlines)

-- | A simple `define preprocessor.  
preprocess :: [(Text, Text)] -> FilePath -> Text -> Text
preprocess env file content = unlines $ pp True [] env $ lines $ content
  where
  pp :: Bool -> [Bool] -> [(Text, Text)] -> [Text] -> [Text]
  pp _ _ _ [] = []
  pp on stack env (a : rest) = case words a of
    "`define" : name : value -> "" : pp on stack (if on then (name, ppLine env $ unwords value) : env else env) rest
    "`ifdef"  : name : _     -> "" : pp (on && (elem    name $ fst $ unzip env)) (on : stack) env rest
    "`ifndef" : name : _     -> "" : pp (on && (notElem name $ fst $ unzip env)) (on : stack) env rest
    "`else" : _
      | not $ null stack     -> "" : pp (head stack && not on) stack env rest
      | otherwise            -> error $ "`else  without associated `ifdef/`ifndef: " ++ file
    "`endif" : _
      | not $ null stack     -> "" : pp (head stack) (tail stack) env rest
      | otherwise            -> error $ "`endif  without associated `ifdef/`ifndef: " ++ file
    _                        -> (if on then ppLine env a else "") : pp on stack env rest

ppLine :: [(Text, Text)] -> Text -> Text
ppLine env as | Just ('`', _) <- uncons as = case lookup name env of
  Just value -> value <> ppLine env rest
  Nothing    -> error $ "Undefined macro: `" ++ unpack name ++ "  Env: " ++ show env
  where
  Just (_, a) = uncons as
  name = takeWhile (flip elem $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['_']) a
  rest = drop (length name) a
ppLine env as | Just (a, b) <- uncons as = a `cons` ppLine env b
ppLine _ _ = mempty
