module Language.Verilog.Parse
  ( parseFile
  ) where

import Text.ParserCombinators.Poly.Plain

import Language.Verilog.Tokens
import Language.Verilog.Types
import Language.Verilog.Lex

parseFile :: FilePath -> String -> [Module]
parseFile file content = case parseTokens tokens of
    Right a -> a
    Left  m -> error m
  where
  tokens = map relocate $ alexScanTokens content
  relocate :: Token -> Token
  relocate (Token t s (Position _ l c)) = Token t s $ Position file l c

parseTokens :: [Token] -> Either String [Module]
parseTokens tokens = case runParser modules tokens of
    (Right a, [])          -> Right a
    (Left msg, t : _) -> Left $ msg ++ "  " ++ show t
    (Left msg, [])         -> Left msg
    (Right _, rest)        -> Left $ "Didn't finish parsing tokens: " ++ show rest

type Verilog = Parser Token

tok :: TokenInfo -> Verilog ()
tok a = satisfy (\ (Token t _ _) -> t == a) >> return ()

identifier :: Verilog Name
identifier = oneOf
  [ satisfy (\ (Token t _ _) -> t == Id_simple ) >>= return . tokenString
  , satisfy (\ (Token t _ _) -> t == Id_escaped) >>= return . tokenString
  , satisfy (\ (Token t _ _) -> t == Id_system ) >>= return . tokenString
  ]

identifiers :: Verilog [Name]
identifiers = commaList identifier

commaList :: Verilog a -> Verilog [a]
commaList item = oneOf
  [ do { a <- item; tok Sym_comma; b <- commaList item; return $ a : b }
  , do { a <- item;                                     return [a]     }
  , do {                                                return []      }
  ]

declaration :: Verilog (Name, Maybe Range)
declaration = do { a <- identifier; b <- optional range; return (a, b) }

declarations :: Verilog [(Name, Maybe Range)]
declarations = commaList declaration

string :: Verilog String
string = satisfy (\ (Token t _ _) -> t == Lit_string ) >>= return . tail . init . tokenString

number :: Verilog String
number = oneOf
  [ satisfy (\ (Token t _ _) -> t == Lit_number_unsigned) >>= return . tokenString
  , satisfy (\ (Token t _ _) -> t == Lit_number         ) >>= return . tokenString
  ]

modules :: Verilog [Module]
modules = do { m <- many1 module_; eof; return m }

module_ :: Verilog Module
module_ = do { tok KW_module; name <- identifier; modulePortList; tok Sym_semi; items <- many1 moduleItem; tok KW_endmodule; return $ Module name items }

modulePortList :: Verilog [Name]
modulePortList = oneOf
  [ do { tok Sym_paren_l; a <- identifiers; tok Sym_paren_r; return a  }
  , do {                                                     return [] }
  ]

moduleItem :: Verilog ModuleItem
moduleItem = oneOf
  [ do { tok KW_parameter; a <- identifier; tok Sym_eq; b <- expr; tok Sym_semi; return $ Paremeter a b }
  , do { a <- net; b <- optional range; c <- declarations;         tok Sym_semi; return $ a b c         }
  ]

range :: Verilog Range
range = do { tok Sym_brack_l; a <- expr; tok Sym_colon; b <- expr; tok Sym_brack_r; return (a, b) }

net :: Verilog (Maybe Range -> [(Name, Maybe Range)] -> ModuleItem)
net = oneOf
  [ do { tok KW_input;  return $ Input  }
  , do { tok KW_output; return $ Output }
  , do { tok KW_inout;  return $ Inout  }
  , do { tok KW_wire;   return $ Wire   }
  , do { tok KW_reg;    return $ Reg    }
  ]

expr :: Verilog Expr
expr = exprInfix >>= return . precedence

exprBasic :: Verilog Expr
exprBasic = oneOf
  [ do { tok Sym_paren_l; a <- expr; tok Sym_paren_r; return a              }
  , do { a <- string;                                 return $ String a     }
  , do { a <- number;                                 return $ Number a     }
  , do { a <- identifier;                             return $ Identifier a }
  , do { tok Sym_bang; a <- expr;                     return $ Not a        }
  ]

exprInfix :: Verilog [Prec]
exprInfix = oneOf
  [ do { a <- exprBasic; op <- operator; rest <- exprInfix; return $ PrecExpr a : op : rest }
  , do { a <- exprBasic; return [PrecExpr a] }
  ]

operator :: Verilog Prec
operator = oneOf
  [ tok Sym_aster   >> return (PrecOp2 Mul)
  , tok Sym_slash   >> return (PrecOp2 Div)
  , tok Sym_percent >> return (PrecOp2 Mod)
  , tok Sym_plus    >> return (PrecOp1 Add)
  , tok Sym_dash    >> return (PrecOp1 Sub)
  ]

type BinOp = Expr -> Expr -> Expr

data Prec
  = PrecExpr Expr
  | PrecOp2  BinOp
  | PrecOp1  BinOp
  | PrecOp0  BinOp

precedence :: [Prec] -> Expr
precedence = f0 . f1 . f2
  where
  f2 :: [Prec] -> [Prec]
  f2 (PrecExpr a : PrecOp2 op : PrecExpr b : rest) = f2 $ PrecExpr (a `op` b) : rest
  f2 (         a :                           rest) = a : f2 rest
  f2 a = a
  f1 :: [Prec] -> [Prec]
  f1 (PrecExpr a : PrecOp1 op : PrecExpr b : rest) = f1 (PrecExpr (a `op` b) : rest)
  f1 (         a :                           rest) = a : f1 rest
  f1 a = a
  f0 :: [Prec] -> Expr
  f0 [PrecExpr a] = a
  f0 (PrecExpr a : PrecOp0 op : PrecExpr b : rest) = f0 (PrecExpr (a `op` b) : rest)
  f0 _ = error "Parse.precedence.f0"

