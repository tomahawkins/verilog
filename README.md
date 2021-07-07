# Verilog parser

Parses Verilog text into an Abstract Syntax Tree (AST). Intended for use with LibreSilicon Compiler (lsc).

## Usage

```haskell

import Data.Text

import Language.Verilog.AST
import Language.Verilog.Parser

newtype Verilog = Verilog { modules :: [Module] }
  deriving (Eq, Show)

parseVerilog :: Text -> Verilog
parseVerilog = Verilog . parseFile [] ""

```
