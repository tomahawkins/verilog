module Language.Verilog.Parser
  ( parseFile
  , preprocess
  ) where

import Data.Text

import Language.Verilog.AST
import Language.Verilog.Parser.Lex
import Language.Verilog.Parser.Parse
import Language.Verilog.Parser.Preprocess
import Language.Verilog.Parser.Tokens

-- | Parses a file given a table of predefined macros, the file name, and the file contents.
parseFile :: [(Text, Text)] -> FilePath -> Text -> [Module]
parseFile env file content = modules tokens
  where
  tokens = fmap relocate $ lexer file $ preprocess env file content
  relocate :: Token -> Token
  relocate (Token t s (Position _ l c)) = Token t s $ Position file l c

