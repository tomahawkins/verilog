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


%left  "+" "-" "!" "~"    -- Not sure how to handle unary operators.
%left  "*" "/" "%"
%left  "<<" ">>"
%left  "<" "<=" ">" ">="
%left  "==" "!=" "===" "!=="
%left  "&" "~&"
%left  "^" "^~"
%left  "|" "~|"
%left  "&&"
%left  "||"
%right "?"

{-
%right "new" "return" "take"
%left "offsetof" "sizeof"
-- these precedences were added to resolve specific conflicts in favor of shifts
-- if things have gone strange, try removing these and seeing if the conflicts
-- that result are related to the problem
%left TYAPPLY
%nonassoc TUPLE WHILE SEPLIST EXPR STEXPR_SP SWITCH IFTHEN 
%left "*"

%left "[" "{"
%nonassoc "]""}"
%left "("
%nonassoc ")"

%nonassoc ";" "," "|"
%nonassoc "="
%right     "||"
%right     "&&"
%nonassoc "@"
%nonassoc ":=" ":!="

%nonassoc symid
%left FNAPPLY "@["
%nonassoc MKGATE

%right "in" "out"
%left ":" ":!"

%nonassoc "!"

%nonassoc "<-"

%left "."
-}

%%

Modules :: { [Module] }
: { [] }
| Modules Module { $1 ++ [$2] }

Module :: { Module }
: "module" { Module "asdf" [] [] }

{

parseError :: [Token] -> a
parseError a = case a of
  [] -> error "Parse error: no tokens left to parse."
  Token _ s p : _ -> error $ "Parse error: unexpected token '" ++ s ++ "' at " ++ show p ++ "."

}
