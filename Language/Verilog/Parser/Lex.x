{
{-# OPTIONS_GHC -w #-}
module Language.Verilog.Parser.Lex
  ( lexer
  ) where

import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Language.Verilog.Parser.Tokens

}

$any     = [.\n\r]
@newline = [\n\r] | \r\n
@comment = "/*" $any* "*/"
         | "//" .* @newline

-- Numbers

$nonZeroDecimalDigit = [1-9]
$decimalDigit = [0-9]
@binaryDigit  = [0-1]
@octalDigit   = [0-7]
@hexDigit     = [0-9a-fA-F]

$sign = [\+\-]

@decimalBase = "'" [dD]
@binaryBase  = "'" [bB]
@octalBase   = "'" [oO]
@hexBase     = "'" [s]? [hH]

@binaryValue         = @binaryDigit ("_" | @binaryDigit)*
@octalValue  = @octalDigit  ("_" | @octalDigit)*
@hexValue    = @hexDigit    ("_" | @hexDigit)*

@unsignedNumber = $decimalDigit ("_" | $decimalDigit)*

@size = $sign? @unsignedNumber

@decimalNumber
  = @unsignedNumber
  | @size? @decimalBase @unsignedNumber

@binaryNumber = @size? @binaryBase @binaryValue
@octalNumber  = @size? @octalBase  @octalValue
@hexNumber    = @size? @hexBase @hexValue
  
-- $exp  = [eE]

-- @realNumber = unsignedNumber "." unsignedNumber | unsignedNumber ( "." unsignedNumber)? exp sign? unsignedNumber
@number = @decimalNumber | @octalNumber | @binaryNumber | @hexNumber

-- Strings

@string = \" [^\r\n]* \"

-- Identifiers

@escapedIdentifier = "\" ($printable # $white)+ $white
@simpleIdentifier  = [a-zA-Z_] [a-zA-Z0-9_\$]*
@systemIdentifier = "$" [a-zA-Z0-9_\$]+


tokens :-

  @comment           ;

  "always"           { tok KW_always     }
  "assign"           { tok KW_assign     }
  "begin"            { tok KW_begin      }
  "case"             { tok KW_case       }
  "default"          { tok KW_default    }
  "else"             { tok KW_else       }
  "end"              { tok KW_end        }
  "endcase"          { tok KW_endcase    }
  "endmodule"        { tok KW_endmodule  }
  "for"              { tok KW_for        }
  "if"               { tok KW_if         }
  "initial"          { tok KW_initial    }
  "inout"            { tok KW_inout      }
  "input"            { tok KW_input      }
  "integer"          { tok KW_integer    }
  "localparam"       { tok KW_localparam }
  "module"           { tok KW_module     }
  "negedge"          { tok KW_negedge    }
  "or"               { tok KW_or         }
  "output"           { tok KW_output     }
  "parameter"        { tok KW_parameter  }
  "posedge"          { tok KW_posedge    }
  "reg"              { tok KW_reg        }
  "wire"             { tok KW_wire       }

  @simpleIdentifier  { tok Id_simple  }
  @escapedIdentifier { tok Id_escaped }
  @systemIdentifier  { tok Id_system  }

  @number            { tok Lit_number }
  @string            { tok Lit_string }

  "("                { tok Sym_paren_l }
  ")"                { tok Sym_paren_r }
  "["                { tok Sym_brack_l }
  "]"                { tok Sym_brack_r }
  "{"                { tok Sym_brace_l }
  "}"                { tok Sym_brace_r }
  "~"                { tok Sym_tildy }
  "!"                { tok Sym_bang }
  "@"                { tok Sym_at }
  "#"                { tok Sym_pound }
  "%"                { tok Sym_percent }
  "^"                { tok Sym_hat }
  "&"                { tok Sym_amp }
  "|"                { tok Sym_bar }
  "*"                { tok Sym_aster }
  "."                { tok Sym_dot }
  ","                { tok Sym_comma }
  ":"                { tok Sym_colon }
  ";"                { tok Sym_semi }
  "="                { tok Sym_eq }
  "<"                { tok Sym_lt }
  ">"                { tok Sym_gt }
  "+"                { tok Sym_plus }
  "-"                { tok Sym_dash }
  "?"                { tok Sym_question }
  "/"                { tok Sym_slash }
  "$"                { tok Sym_dollar }
  "'"                { tok Sym_s_quote }

  "~&"               { tok Sym_tildy_amp }
  "~|"               { tok Sym_tildy_bar }
  "~^"               { tok Sym_tildy_hat }
  "^~"               { tok Sym_hat_tildy }
  "=="               { tok Sym_eq_eq }
  "!="               { tok Sym_bang_eq }
  "&&"               { tok Sym_amp_amp }
  "||"               { tok Sym_bar_bar }
  "**"               { tok Sym_aster_aster }
  "<="               { tok Sym_lt_eq }
  ">="               { tok Sym_gt_eq }
  ">>"               { tok Sym_gt_gt }
  "<<"               { tok Sym_lt_lt }
  "++"               { tok Sym_plus_plus }
  "--"               { tok Sym_dash_dash }
  "+="               { tok Sym_plus_eq }
  "-="               { tok Sym_dash_eq }
  "*="               { tok Sym_aster_eq }
  "/="               { tok Sym_slash_eq }
  "%="               { tok Sym_percent_eq }
  "&="               { tok Sym_amp_eq }
  "|="               { tok Sym_bar_eq }
  "^="               { tok Sym_hat_eq }
  "+:"               { tok Sym_plus_colon }
  "-:"               { tok Sym_dash_colon }
  "::"               { tok Sym_colon_colon }
  ".*"               { tok Sym_dot_aster }
  "->"               { tok Sym_dash_gt }
  ":="               { tok Sym_colon_eq }
  ":/"               { tok Sym_colon_slash }
  "##"               { tok Sym_pound_pound }
  "[*"               { tok Sym_brack_l_aster }
  "[="               { tok Sym_brack_l_eq }
  "=>"               { tok Sym_eq_gt }
  "@*"               { tok Sym_at_aster }
  "(*"               { tok Sym_paren_l_aster }
  "*)"               { tok Sym_aster_paren_r }
  "*>"               { tok Sym_aster_gt }

  "==="              { tok Sym_eq_eq_eq }
  "!=="              { tok Sym_bang_eq_eq }
  "=?="              { tok Sym_eq_question_eq }
  "!?="              { tok Sym_bang_question_eq }
  ">>>"              { tok Sym_gt_gt_gt }
  "<<<"              { tok Sym_lt_lt_lt }
  "<<="              { tok Sym_lt_lt_eq }
  ">>="              { tok Sym_gt_gt_eq }
  "|->"              { tok Sym_bar_dash_gt }
  "|=>"              { tok Sym_bar_eq_gt }
  "[->"              { tok Sym_brack_l_dash_gt }
  "@@("              { tok Sym_at_at_paren_l }
  "(*)"              { tok Sym_paren_l_aster_paren_r }
  "->>"              { tok Sym_dash_gt_gt }
  "&&&"              { tok Sym_amp_amp_amp }

  "<<<="             { tok Sym_lt_lt_lt_eq }
  ">>>="             { tok Sym_gt_gt_gt_eq }

  $white             ;

  .                  { tok Unknown }

{
tok :: TokenName -> AlexPosn -> String -> Token
tok t (AlexPn _ l c) s = Token t s $ Position "" l c

lexer :: String -> T.Text -> [Token]
lexer file text = go (alexStartPos, '\n', text `T.snoc` '\n')
  where
    go inp@(pos, _, cs) = case {-# SCC "alexScan" #-} alexScan inp 0 of
        AlexEOF                -> []
        AlexError inp'         -> error (errMsg inp')
        AlexSkip  inp'   _     -> go inp'
        AlexToken inp' len act -> act pos (T.unpack $ T.take len cs) : go inp'

    errMsg (AlexPn _ line col, _, cs) =
        file ++ ": lexical error (line " ++ show line ++ ", col " ++ show col ++ ")\n"
             ++ "    near " ++ show (T.unpack $ T.take 40 cs)

-----------------------------------------------------------

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  T.Text)       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,_,cs) | T.null cs  = Nothing
                     | {-# SCC "alexSkip" #-} alexSkip c = alexGetChar (p', c, cs')
                     | otherwise  = p' `seq` cs' `seq` Just (c, (p', c, cs'))
  where
    c   = T.head cs
    cs' = T.tail cs
    p'  = alexMove p c

alexGetByte :: AlexInput -> Maybe (Int,AlexInput)
alexGetByte i = case alexGetChar i of
  Nothing -> Nothing
  Just (c, j) -> Just (ord c, j)

alexSkip :: Char -> Bool
alexSkip '\xFEFF' = True
alexSkip _        = False

-----------------------------------------------------------

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)
}


