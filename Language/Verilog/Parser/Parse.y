{
module Language.Verilog.Parser.Parse (modules) where

import Language.Verilog.AST
import Language.Verilog.Parser.Tokens
}

%name modules
%tokentype { Token }
%error { parseError }
-- %monad { Either String }

-- There are 0 shift/reduce conflicts here that we know about.
%expect 0

%token

"always"           { Token KW_always     _ _ }
"assign"           { Token KW_assign     _ _ }
"begin"            { Token KW_begin      _ _ }
"case"             { Token KW_case       _ _ }
"default"          { Token KW_default    _ _ }
"else"             { Token KW_else       _ _ }
"end"              { Token KW_end        _ _ }
"endcase"          { Token KW_endcase    _ _ }
"endmodule"        { Token KW_endmodule  _ _ }
"for"              { Token KW_for        _ _ }
"if"               { Token KW_if         _ _ }
"initial"          { Token KW_initial    _ _ }
"inout"            { Token KW_inout      _ _ }
"input"            { Token KW_input      _ _ }
"integer"          { Token KW_integer    _ _ }
"module"           { Token KW_module     _ _ }
"negedge"          { Token KW_negedge    _ _ }
"or"               { Token KW_or         _ _ }
"output"           { Token KW_output     _ _ }
"parameter"        { Token KW_parameter  _ _ }
"posedge"          { Token KW_posedge    _ _ }
"reg"              { Token KW_reg        _ _ }
"wire"             { Token KW_wire       _ _ }

simpleIdentifier   { Token Id_simple     _ _ }
escapedIdentifier  { Token Id_escaped    _ _ }
systemIdentifier   { Token Id_system     _ _ }
number             { Token Lit_number    _ _ }
string             { Token Lit_string    _ _ }

"("                { Token Sym_paren_l _ _ }
")"                { Token Sym_paren_r _ _ }
"["                { Token Sym_brack_l _ _ }
"]"                { Token Sym_brack_r _ _ }
"{"                { Token Sym_brace_l _ _ }
"}"                { Token Sym_brace_r _ _ }
"~"                { Token Sym_tildy _ _ }
"!"                { Token Sym_bang _ _ }
"@"                { Token Sym_at _ _ }
"#"                { Token Sym_pound _ _ }
"%"                { Token Sym_percent _ _ }
"^"                { Token Sym_hat _ _ }
"&"                { Token Sym_amp _ _ }
"|"                { Token Sym_bar _ _ }
"*"                { Token Sym_aster _ _ }
"."                { Token Sym_dot _ _ }
","                { Token Sym_comma _ _ }
":"                { Token Sym_colon _ _ }
";"                { Token Sym_semi _ _ }
"="                { Token Sym_eq _ _ }
"<"                { Token Sym_lt _ _ }
">"                { Token Sym_gt _ _ }
"+"                { Token Sym_plus _ _ }
"-"                { Token Sym_dash _ _ }
"?"                { Token Sym_question _ _ }
"/"                { Token Sym_slash _ _ }
"$"                { Token Sym_dollar _ _ }
"'"                { Token Sym_s_quote _ _ }
"~&"               { Token Sym_tildy_amp _ _ }
"~|"               { Token Sym_tildy_bar _ _ }
"~^"               { Token Sym_tildy_hat _ _ }
"^~"               { Token Sym_hat_tildy _ _ }
"=="               { Token Sym_eq_eq _ _ }
"!="               { Token Sym_bang_eq _ _ }
"&&"               { Token Sym_amp_amp _ _ }
"||"               { Token Sym_bar_bar _ _ }
"**"               { Token Sym_aster_aster _ _ }
"<="               { Token Sym_lt_eq _ _ }
">="               { Token Sym_gt_eq _ _ }
">>"               { Token Sym_gt_gt _ _ }
"<<"               { Token Sym_lt_lt _ _ }
"++"               { Token Sym_plus_plus _ _ }
"--"               { Token Sym_dash_dash _ _ }
"+="               { Token Sym_plus_eq _ _ }
"-="               { Token Sym_dash_eq _ _ }
"*="               { Token Sym_aster_eq _ _ }
"/="               { Token Sym_slash_eq _ _ }
"%="               { Token Sym_percent_eq _ _ }
"&="               { Token Sym_amp_eq _ _ }
"|="               { Token Sym_bar_eq _ _ }
"^="               { Token Sym_hat_eq _ _ }
"+:"               { Token Sym_plus_colon _ _ }
"-:"               { Token Sym_dash_colon _ _ }
"::"               { Token Sym_colon_colon _ _ }
".*"               { Token Sym_dot_aster _ _ }
"->"               { Token Sym_dash_gt _ _ }
":="               { Token Sym_colon_eq _ _ }
":/"               { Token Sym_colon_slash _ _ }
"##"               { Token Sym_pound_pound _ _ }
"[*"               { Token Sym_brack_l_aster _ _ }
"[="               { Token Sym_brack_l_eq _ _ }
"=>"               { Token Sym_eq_gt _ _ }
"@*"               { Token Sym_at_aster _ _ }
"(*"               { Token Sym_paren_l_aster _ _ }
"*)"               { Token Sym_aster_paren_r _ _ }
"*>"               { Token Sym_aster_gt _ _ }
"==="              { Token Sym_eq_eq_eq _ _ }
"!=="              { Token Sym_bang_eq_eq _ _ }
"=?="              { Token Sym_eq_question_eq _ _ }
"!?="              { Token Sym_bang_question_eq _ _ }
">>>"              { Token Sym_gt_gt_gt _ _ }
"<<<"              { Token Sym_lt_lt_lt _ _ }
"<<="              { Token Sym_lt_lt_eq _ _ }
">>="              { Token Sym_gt_gt_eq _ _ }
"|->"              { Token Sym_bar_dash_gt _ _ }
"|=>"              { Token Sym_bar_eq_gt _ _ }
"[->"              { Token Sym_brack_l_dash_gt _ _ }
"@@("              { Token Sym_at_at_paren_l _ _ }
"(*)"              { Token Sym_paren_l_aster_paren_r _ _ }
"->>"              { Token Sym_dash_gt_gt _ _ }
"&&&"              { Token Sym_amp_amp_amp _ _ }
"<<<="             { Token Sym_lt_lt_lt_eq _ _ }
">>>="             { Token Sym_gt_gt_gt_eq _ _ }


%right "?"
%left  "||"
%left  "&&"
%left  "|" "~|"
%left  "^" "^~"
%left  "&" "~&"
%left  "==" "!=" "===" "!=="
%left  "<" "<=" ">" ">="
%left  "<<" ">>"
%left  "+" "-"
%left  "*" "/" "%"
%left  UPlus UMinus "!" "~"    -- XXX Need to handle unary operators.

%%

Modules :: { [Module] }
:                { [] }
| Modules Module { $1 ++ [$2] }

Module :: { Module }
: "module" Identifier ModulePortList ";" ModuleItems "endmodule"{ Module $2 $3 $5 }

Identifier :: { Identifier }
: simpleIdentifier   { tokenString $1 }
| escapedIdentifier  { tokenString $1 }
| systemIdentifier   { tokenString $1 }

ModulePortList :: { [Identifier] }
:                         { [] }
| "(" ModulePortList1 ")" { $2 }

ModulePortList1 :: { [Identifier] }
:                     Identifier  { [$1] }
| ModulePortList1 "," Identifier  { $1 ++ [$3] }

ModuleItems :: { [ModuleItem] }
:                         { [] }
| ModuleItems ModuleItem  { $1 ++ [$2] }

ModuleItem :: { ModuleItem }
: "parameter" MaybeRange Identifier "=" Expr ";"        { Parameter $2 $3 $5 }
| Net MaybeRange Declarations ";"                       { $1 $2 $3 }
| "assign" LHS "=" Expr ";"                             { Assign $2 $4 }
| "initial" Stmt                                        { Initial $2 }
| "always" "@" "(" Sense ")" Stmt                       { Always $4 $6 }
| Identifier ParameterBindings Identifier Bindings ";"  { Instance $1 $2 $3 $4 }

Net :: { Maybe Range -> [(Identifier, Maybe Range)] -> ModuleItem }
: "input"   { Input   }
| "output"  { Output  }
| "inout"   { Inout   }
| "wire"    { Wire    }
| "reg"     { Reg     }

Declarations :: { [(Identifier, Maybe Range)] }
:                  Identifier MaybeRange    { [($1, $2)]       }
| Declarations "," Identifier MaybeRange    { $1 ++ [($3, $4)] }

MaybeRange :: { Maybe Range }
:         { Nothing }
| Range   { Just $1 }

Range :: { Range }
: "[" Expr ":" Expr "]"  { ($2, $4) }

LHS :: { LHS }
: Identifier              { LHS      $1    }
| Identifier Range        { LHSRange $1 $2 }
| Identifier "[" Expr "]" { LHSBit   $1 $3 }

Sense :: { Sense }
:            Sense1 { $1 }
| Sense "or" Sense1 { SenseOr $1 $3 }

Sense1 :: { Sense }
:           LHS { Sense        $1 }
| "posedge" LHS { SensePosedge $2 }
| "negedge" LHS { SenseNegedge $2 }

Bindings :: { [(Identifier, Maybe Expr)] }
: "(" Bindings1 ")" { $2 }

Bindings1 :: { [(Identifier, Maybe Expr)] }
:               Binding  { [$1] }
| Bindings1 "," Binding  { $1 ++ [$3] }

Binding :: { (Identifier, Maybe Expr) }
: "." Identifier "(" MaybeExpr ")" { ($2, $4) }

ParameterBindings :: { [(Identifier, Maybe Expr)] }
:              { [] }
| "#" Bindings { $2 }

MaybeExpr :: { Maybe Expr }
:         { Nothing }
| Expr    { Just $1 }

Expr :: { Expr }
: string { String "asdf" }

Stmt :: { Stmt }
: ";" { Null }

  {-
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
-}



{
parseError :: [Token] -> a
parseError a = case a of
  []              -> error "Parse error: no tokens left to parse."
  Token _ s p : _ -> error $ "Parse error: unexpected token '" ++ s ++ "' at " ++ show p ++ "."
}

