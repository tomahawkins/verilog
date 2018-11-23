{
module Language.Verilog.Parser.Parse (ast) where

import Data.Bits
import Data.List
import qualified Data.Text as T

import Data.BitVec
import Language.Verilog.AST
import Language.Verilog.Parser.Tokens
}

%name ast
%tokentype { Token }
%error { parseError }

%token

"always"           { Token KW_always     _ _ }
"assign"           { Token KW_assign     _ _ }
"begin"            { Token KW_begin      _ _ }
"case"             { Token KW_case       _ _ }
"casez"            { Token KW_casez      _ _ }
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
"localparam"       { Token KW_localparam _ _ }
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

%nonassoc NoElse
%nonassoc "else"
%right "?" ":"
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
%left  UPlus UMinus "!" "~"


%%

Ast :: { [Module] }
:            { [] }
| Ast Module { $1 ++ [$2] }

Module :: { Module }
: "module" Identifier ModulePortList ";" many(ModuleItem) "endmodule" { Module $2 $3 $5 }

Identifier :: { Identifier }
: simpleIdentifier   { tokenString $1 }
| escapedIdentifier  { tokenString $1 }
| systemIdentifier   { tokenString $1 }

ModulePortList :: { [Identifier] }
:                         { [] }
| "("                 ")" { [] }
| "(" ListOfPorts ")" { $2 }

ListOfPorts :: { [Identifier] }
:                 PortDirection opt(Range) Identifier  { [$3] }
| ListOfPorts "," PortDirection opt(Range) Identifier  { $1 ++ [$5] }

PortDirection :: { () }
: "input"  { () }
| "output" { () }

ModuleItem :: { ModuleItem }
: "parameter"  MaybeRange Identifier "=" Expr ";"       { Parameter  $2 $3 $5 }
| "localparam" MaybeRange Identifier "=" Expr ";"       { Localparam $2 $3 $5 }
| "input"  MaybeRange sepBy1(Identifier, ",") ";"       { Input  $2 $3 }
| "output" MaybeRange sepBy1(Identifier, ",") ";"       { Output $2 $3 }
| "inout"  MaybeRange sepBy1(Identifier, ",") ";"       { Inout  $2 $3 }
| "reg"    MaybeRange RegDeclaration ";"                { Reg    $2 $3 }
| "wire"   MaybeRange sepBy1(WireDeclaration, ",") ";"  { Wire   $2 $3 }
| "integer" sepBy1(Identifier, ",") ";"                 { Integer $2 }
| "assign" LHS "=" Expr ";"                             { Assign $2 $4 }
| "initial" Stmt                                        { Initial $2 }
| "always"                   Stmt                       { Always Nothing $2 }
| "always" "@" "(" Sense ")" Stmt                       { Always (Just $4) $6 }
| Identifier ParameterBindings Identifier Bindings ";"  { Instance $1 $2 $3 $4 }

RegDeclaration :: { [(Identifier, Maybe Range)] }
:                    Identifier MaybeRange    { [($1, $2)]       }
| RegDeclaration "," Identifier MaybeRange    { $1 ++ [($3, $4)] }

WireDeclaration :: { (Identifier, Maybe Expr) }
: Identifier               { ($1, Nothing) }
| Identifier "=" Expr      { ($1, Just $3) }

MaybeRange :: { Maybe Range }
:         { Nothing }
| Range   { Just $1 }

Range :: { Range }
: "[" Expr ":" Expr "]"  { ($2, $4) }

LHS :: { LHS }
: Identifier              { LHS       $1    }
| Identifier Range        { LHSRange  $1 $2 }
| Identifier "[" Expr "]" { LHSBit    $1 $3 }
| "{" LHSs "}"            { LHSConcat $2 }

LHSs :: { [LHS] }
:          LHS  { [$1] } 
| LHSs "," LHS  { $1 ++ [$3] }

Sense :: { Sense }
:            Sense1 { $1 }
| Sense "or" Sense1 { SenseOr $1 $3 }

Sense1 :: { Sense }
:           LHS { Sense        $1 }
| "posedge" LHS { SensePosedge $2 }
| "negedge" LHS { SenseNegedge $2 }

Bindings :: { [(Identifier, Maybe Expr)] }
: "(" sepBy1(Binding, ",") ")" { $2 }

Binding :: { (Identifier, Maybe Expr) }
: "." Identifier "(" opt(Expr) ")" { ($2, $4) }
| "." Identifier                   { ($2, Just $ Ident $2) }

ParameterBindings :: { [(Identifier, Maybe Expr)] }
:              { [] }
| "#" Bindings { $2 }

Stmt :: { Stmt }
: ";" { Null }
| "begin"                many(Stmt) "end"               { Block Nothing   $2 }
| "begin" ":" Identifier many(Stmt) "end"               { Block (Just $3) $4 }
| "reg" MaybeRange RegDeclaration ";"                   { StmtReg $2 $3      }
| "integer" sepBy1(Identifier, ",") ";"                 { StmtInteger $2     }
| "if" "(" Expr ")" Stmt "else" Stmt                    { If $3 $5 $7        }
| "if" "(" Expr ")" Stmt %prec NoElse                   { If $3 $5 Null      }
| "for" "(" Identifier "=" Expr ";" Expr ";" Identifier "=" Expr ")" Stmt { For ($3, $5) $7 ($9, $11) $13 }
| LHS "=" Expr ";"                                      { BlockingAssignment $1 $3 }
| LHS "<=" Expr ";"                                     { NonBlockingAssignment $1 $3 }
| "#" Expr Stmt                                         { Delay $2 $3 }
| Call ";"                                              { StmtCall $1 }
| "case"  "(" Expr ")" many(Case) CaseDefault "endcase" { Case $3 $5 $6 }

Case :: { Case }
: sepBy1(Expr, ",") ":" Stmt  { ($1, $3) }

CaseDefault  :: { Maybe Stmt }
:                     { Nothing }
| "default" ":" Stmt  { Just $3 }

Number :: { BitVec }
: number    { toNumber $1 }

String :: { String }
: string    { toString $1 }

Call :: { Call }
: Identifier "(" CallArgs ")"  { Call $1 $3 }
| Identifier  { Call $1 [] }

CallArgs :: { [Expr] }
CallArgs
:              Expr  { [$1] }
| CallArgs "," Expr  { $1 ++ [$3] }

Expr :: { Expr }
: "(" Expr ")"                { $2 }
| String                      { String $1 }
| Number                      { Number $1 }
| Call                        { ExprCall $1 }
| Identifier                  { Ident      $1    }
| Identifier Range            { IdentRange $1 $2 }
| Identifier "[" Expr "]"     { IdentBit   $1 $3 }
| "{" Expr "{" sepBy1(Expr, ",") "}" "}"  { Repeat $2 $4 }
| "{" sepBy1(Expr, ",") "}"               { Concat $2 }
| Expr "?" Expr ":" Expr      { Mux $1 $3 $5 }
| Expr "||" Expr              { BinOp Or  $1 $3 }
| Expr "&&" Expr              { BinOp And $1 $3 }
| Expr "|"  Expr              { BinOp BWOr $1 $3 }
| Expr "^"  Expr              { BinOp BWXor $1 $3 }
| Expr "&"  Expr              { BinOp BWAnd $1 $3 }
| Expr "==" Expr              { BinOp Eq $1 $3 }
| Expr "!=" Expr              { BinOp Ne $1 $3 }
| Expr "<"  Expr              { BinOp Lt $1 $3 }
| Expr "<=" Expr              { BinOp Le $1 $3 }
| Expr ">"  Expr              { BinOp Gt $1 $3 }
| Expr ">=" Expr              { BinOp Ge $1 $3 }
| Expr "<<" Expr              { BinOp ShiftL $1 $3 }
| Expr ">>" Expr              { BinOp ShiftR $1 $3 }
| Expr ">>>" Expr             { BinOp ShiftR $1 $3 }
| Expr "+"  Expr              { BinOp Add $1 $3 }
| Expr "-"  Expr              { BinOp Sub $1 $3 }
| Expr "*"  Expr              { BinOp Mul $1 $3 }
| Expr"/"   Expr              { BinOp Div $1 $3 }
| Expr "%"  Expr              { BinOp Mod $1 $3 }
| "!" Expr                    { UniOp Not $2 }
| "~" Expr                    { UniOp BWNot $2 }
| "+" Expr %prec UPlus        { UniOp UAdd $2 }
| "-" Expr %prec UMinus       { UniOp USub $2 }


sepBy1(p, s)
: sepBy1(p, s) s p { $3 : $1 }
| p { [$1] }

many(p)
: many(p) p { $2 : $1 }
| { [] } 

opt(p)
: p { Just $1 }
|   { Nothing }

{
parseError :: [Token] -> a
parseError a = case a of
  []              -> error "Parse error: no tokens left to parse."
  Token t s p : _ -> error $ "Parse error: unexpected token '" ++ s ++ "' (" ++ show t ++ ") at " ++ show p ++ "."

toString :: Token -> String
toString = tail . init . tokenString

toNumber :: Token -> BitVec
toNumber = number . tokenString
  where
  number :: String -> BitVec
  number a
    | all (flip elem ['0' .. '9']) a = fromInteger $ read a
    | head a == '\''                 = fromInteger $ f a
    | isInfixOf  "'"  a              = bitVec (read w) (f b)
    | otherwise                      = error $ "Invalid number format: " ++ a
    where
    w = takeWhile (/= '\'') a
    b = dropWhile (/= '\'') a
    f a 
      | isPrefixOf "'d" a = read $ drop 2 a
      | isPrefixOf "'h" a = read $ "0x" ++ drop 2 a
      | isPrefixOf "'sh" a = read $ "0x" ++ drop 3 a
      | isPrefixOf "'b" a = foldl (\ n b -> shiftL n 1 .|. (if b == '1' then 1 else 0)) 0 (drop 2 a)
      | otherwise         = error $ "Invalid number format: " ++ a

}

