module Language.Verilog.Parse
  ( parseFile
  ) where

import Text.ParserCombinators.Poly.Plain

import Language.Verilog.Tokens
import Language.Verilog.Types
import Language.Verilog.Lex
import Language.Verilog.Preprocess

-- | Parses a file given a table of predefined macros, the file name, and the file contents.
parseFile :: [(String, String)] -> FilePath -> String -> [Module]
parseFile env file content = case parseTokens tokens of
    Right a -> a
    Left  m -> error m
  where
  tokens = map relocate $ alexScanTokens $ preprocess env file content
  relocate :: Token -> Token
  relocate (Token t s (Position _ l c)) = Token t s $ Position file l c

parseTokens :: [Token] -> Either String [Module]
parseTokens tokens = case runParser modules tokens of
    (Right a, [])          -> Right a
    (Left msg, t : _) -> Left $ msg ++ "  " ++ show t
    (Left msg, [])         -> Left msg
    (Right _, rest)        -> Left $ "Didn't finish parsing tokens: " ++ show rest

type Verilog = Parser Token

tok :: TokenName -> Verilog ()
tok a = satisfy (\ (Token t _ _) -> t == a) >> return ()

identifier :: Verilog Identifier
identifier = oneOf
  [ satisfy (\ (Token t _ _) -> t == Id_simple ) >>= return . tokenString
  , satisfy (\ (Token t _ _) -> t == Id_escaped) >>= return . tokenString
  , satisfy (\ (Token t _ _) -> t == Id_system ) >>= return . tokenString
  ]

commaList :: Verilog a -> Verilog [a]
commaList item = oneOf
  [ do { a <- item; tok Sym_comma; b <- commaList item; return $ a : b }
  , do { a <- item;                                     return [a]     }
  ]

declaration :: Verilog (Identifier, Maybe Range)
declaration = do { a <- identifier; b <- optional range; return (a, b) }

declarations :: Verilog [(Identifier, Maybe Range)]
declarations = commaList declaration

string :: Verilog String
string = satisfy (\ (Token t _ _) -> t == Lit_string ) >>= return . tail . init . tokenString

number :: Verilog String
number = satisfy (\ (Token t _ _) -> t == Lit_number) >>= return . tokenString

modules :: Verilog [Module]
modules = do { m <- many module_; eof; return m }

module_ :: Verilog Module
module_ = do { tok KW_module; name <- identifier; ports <- modulePortList; tok Sym_semi; items <- many moduleItem; tok KW_endmodule; return $ Module name ports items }

modulePortList :: Verilog [Identifier]
modulePortList = oneOf
  [ do { tok Sym_paren_l; a <- commaList identifier; tok Sym_paren_r; return a  }
  , return []
  ]

moduleItem :: Verilog ModuleItem
moduleItem = oneOf
  [ do { tok KW_parameter; commit $ do { a <- optional range; b <- identifier; tok Sym_eq; c <- expr; tok Sym_semi; return $ Paremeter a b c } }
  , do { a <- net;         commit $ do { b <- optional range; c <- declarations; tok Sym_semi; return $ a b c         } }
  , do { tok KW_assign;    commit $ do { a <- lhs; tok Sym_eq; b <- expr;        tok Sym_semi; return $ Assign a b    } }
  , do { tok KW_initial;   commit $ do { a <- stmt;                                                           return $ Initial a    } }
  , do { tok KW_always;    commit $ do { tok Sym_at; tok Sym_paren_l; a <- sense; tok Sym_paren_r; b <- stmt; return $ Always a b   } }
  , do { a <- identifier; b <- parameterBindings; c <- identifier; d <- signalBindings; tok Sym_semi; return $ Instance a b c d }
  ]

parameterBindings :: Verilog [(Identifier, Maybe Expr)]
parameterBindings = oneOf
  [ do { tok Sym_pound; signalBindings }
  , return []
  ]

signalBindings :: Verilog [(Identifier, Maybe Expr)]
signalBindings = do { tok Sym_paren_l; a <- commaList binding; tok Sym_paren_r; return a }

binding :: Verilog (Identifier, Maybe Expr)
binding = do { tok Sym_dot; a <- identifier; tok Sym_paren_l; b <- optional expr; tok Sym_paren_r; return (a, b) }

range :: Verilog Range
range = do { tok Sym_brack_l; a <- expr; tok Sym_colon; b <- expr; tok Sym_brack_r; return (a, b) }

bit :: Verilog Expr
bit = do { tok Sym_brack_l; a <- expr; tok Sym_brack_r; return a }

net :: Verilog (Maybe Range -> [(Identifier, Maybe Range)] -> ModuleItem)
net = oneOf
  [ do { tok KW_input;  return $ Input  }
  , do { tok KW_output; return $ Output }
  , do { tok KW_inout;  return $ Inout  }
  , do { tok KW_wire;   return $ Wire   }
  , do { tok KW_reg;    return $ Reg    }
  ]

expr :: Verilog Expr
expr = oneOf
  [ do { a <- expr1; tok Sym_question; b <- expr; tok Sym_colon; c <- expr; return $ Mux a b c }
  , expr1
  ]
  where
  expr1 :: Verilog Expr
  expr1 = exprBinOp operators

  exprBinOp :: [Verilog (Expr -> Expr -> Expr)] -> Verilog Expr
  exprBinOp [] = exprTop
  exprBinOp (op : rest) = chainl1 (exprBinOp rest) op

-- | Operators with precedence, low to high.
operators :: [Verilog (Expr -> Expr -> Expr)]
operators = reverse [ oneOf [ tok t >> return op | (t, op) <- ops ] | ops <- ops ]
  where
  ops =
    [ [(Sym_aster, Mul), (Sym_slash, Div), (Sym_percent, Mod)]
    , [(Sym_plus, Add), (Sym_dash, Sub)]
    , [(Sym_lt_lt, ShiftL), (Sym_gt_gt, ShiftR)]
    , [(Sym_lt, Lt), (Sym_lt_eq, Le), (Sym_gt, Gt), (Sym_gt_eq, Ge)]
    , [(Sym_eq_eq, Eq), (Sym_bang_eq, Ne)]
    , [(Sym_amp, BWAnd)]
    , [(Sym_hat, BWXor)]
    , [(Sym_bar, BWOr)]
    , [(Sym_amp_amp, And)]
    , [(Sym_bar_bar, Or)]
    ]

-- Verilog operator precedence.
--  11  + - ! ~ (unary)
--  10  * / %          
--   9  + - (binary)   
--   8  << >>          
--   7  < <= > >=      
--   6  == != === !==  
--   5  & ~&           
--   4  ^ ^~           
--   3  | ~|           
--   2  &&             
--   1  ||             
--   0  ?:

chainl1 :: Verilog Expr -> Verilog (Expr -> Expr -> Expr) -> Verilog Expr
chainl1 p op = do { x <- p; rest x }
  where
  rest x = oneOf
    [ do { f <- op; y <- p; rest $ f x y }
    , return x
    ]

exprTop :: Verilog Expr
exprTop = oneOf
  [ do { a <- call;       return $ ExprCall a }
  , do { a <- string;     return $ String   a }
  , do { a <- number;     return $ Number   a }
  , do { a <- lhs;        return $ ExprLHS  a }
  , do { tok Sym_paren_l; a <- expr; tok Sym_paren_r; return a }
  , do { tok Sym_bang;    a <- expr;                  return $ Not a   }
  , do { tok Sym_tildy;   a <- expr;                  return $ BWNot a }
  , do { tok Sym_brace_l; a <- expr; tok Sym_brace_l; b <- commaList expr; tok Sym_brace_r; tok Sym_brace_r; return $ Repeat a b }
  , do { tok Sym_brace_l; a <- commaList expr; tok Sym_brace_r; return $ Concat a }
  ]

stmt :: Verilog Stmt
stmt = oneOf
  [ do { tok KW_begin; a <- optional (tok Sym_colon >> identifier); b <- many stmt; tok KW_end; return $ Block a b }
  , do { tok KW_for; tok Sym_paren_l; a <- identifier; tok Sym_eq; b <- expr; tok Sym_semi; c <- expr; tok Sym_semi; d <- identifier; tok Sym_eq; e <- expr; tok Sym_paren_r; f <- stmt; return $ For (a, b) c (d, e) f }
  , do { tok KW_integer; a <- identifier;        tok Sym_semi; return $ Integer a }
  , do { tok KW_if; tok Sym_paren_l; a <- expr; tok Sym_paren_r; b <- stmt; tok KW_else; c <- stmt; return $ If a b c    }
  , do { tok KW_if; tok Sym_paren_l; a <- expr; tok Sym_paren_r; b <- stmt;                         return $ If a b Null }
  , do { a <- lhs; tok Sym_eq;    b <- expr;     tok Sym_semi; return $ BlockingAssignment    a b }
  , do { a <- lhs; tok Sym_lt_eq; b <- expr;     tok Sym_semi; return $ NonBlockingAssignment a b }
  , do { a <- call; tok Sym_semi; return $ StmtCall a }
  , do { tok KW_case; tok Sym_paren_l; a <- expr; tok Sym_paren_r; b <- many case_; c <- default_; tok KW_endcase; return $ Case a b c }
  , do { tok Sym_semi; return Null }
  , do { tok Sym_pound; a <- number; b <- stmt; return $ Delay a b }
  ]

call :: Verilog Call
call = do { a <- identifier; tok Sym_paren_l; b <- commaList expr; tok Sym_paren_r; return $ Call a b }

case_ :: Verilog Case
case_ = do { a <- commaList expr; tok Sym_colon; b <- stmt; return (a, b) }

default_ :: Verilog Stmt
default_ = oneOf
  [ do { tok KW_default; tok Sym_colon; stmt }
  , return Null
  ]

sense :: Verilog Sense
sense = oneOf
  [ do { a <- sense'; tok KW_or; b <- sense; return $ SenseOr a b }
  , do { a <- sense';                        return a             }
  ]
  where
  sense' :: Verilog Sense
  sense' = oneOf
    [ do { tok KW_posedge; a <- lhs; return $ SensePosedge a }
    , do { tok KW_negedge; a <- lhs; return $ SenseNegedge a }
    , do {                 a <- lhs; return $ Sense        a }
    ]

lhs :: Verilog LHS
lhs = oneOf
  [ do { a <- identifier; b <- range; return $ LHSRange a b }
  , do { a <- identifier; b <- bit;   return $ LHSBit   a b }
  , do { a <- identifier;             return $ LHS      a   }
  ]

