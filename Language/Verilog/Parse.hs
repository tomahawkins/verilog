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

{-
integer :: Asm Integer
integer = do
  (T.IntegerConst a, _) <- satisfy $ \ (a, _) -> case a of { T.IntegerConst _ -> True; _ -> False }
  -}

identifier :: Verilog Name
identifier = oneOf
  [ satisfy (\ (Token t _ _) -> t == Id_simple ) >>= return . tokenString
  , satisfy (\ (Token t _ _) -> t == Id_escaped) >>= return . tokenString
  , satisfy (\ (Token t _ _) -> t == Id_system ) >>= return . tokenString
  ]

identifiers :: Verilog [Name]
identifiers = oneOf
  [ do { a <- identifier; tok Sym_comma; b <- identifiers; return $ a : b }
  , do { a <- identifier;                                  return [a]     }
  , do {                                                   return []      }
  ]

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
  , do { a <- net; b <- bus; c <- identifiers;                     tok Sym_semi; return $ a b c         }
  ]

expr :: Verilog Expr
expr = oneOf
  [ do { a <- string;     return $ String a     }
  , do { a <- number;     return $ Number a     }
  , do { a <- identifier; return $ Identifier a }
  ]

bus :: Verilog (Maybe (Expr, Expr))
bus = oneOf
  [ do { tok Sym_brack_l; a <- expr; tok Sym_colon; b <- expr; tok Sym_brack_r; return $ Just (a, b) }
  , do {                                                                        return $ Nothing     }
  ]

net :: Verilog (Maybe (Expr, Expr) -> [Name] -> ModuleItem)
net = oneOf
  [ do { tok KW_input;  return $ Input  }
  , do { tok KW_output; return $ Output }
  , do { tok KW_inout;  return $ Inout  }
  , do { tok KW_wire;   return $ Wire   }
  , do { tok KW_reg;    return $ Reg    }
  ]

{-

parseVerilog :: String -> IO ()
parseVerilog verilog = parseTest source_text $ concatMap lexSection $ deline verilog

lexSection :: (String,Int,String) -> [Token]
lexSection (file,line,code) = map reposition (alexScanTokens code)
  where
    reposition :: Token -> Token
    reposition (Token t (_,l,c) s) = Token t (file, line + l - 1, c) s


type VerilogParser a = GenParser Token () a

posToken :: Token -> SourcePos
posToken (Token _ (f,l,c) _) = newPos f l c

tokStr :: TokenInfo -> VerilogParser String
tokStr tokenInfo = token show posToken (\ (Token tokenInfo' _ s) -> if tokenInfo == tokenInfo' then Just s else Nothing)

tokPosStr :: TokenInfo -> VerilogParser (Position,String)
tokPosStr tokenInfo = token show posToken (\ (Token tokenInfo' p s) -> if tokenInfo == tokenInfo' then Just (p,s) else Nothing)

tok_ :: TokenInfo -> VerilogParser ()
tok_ tokenInfo = token show posToken (\ (Token tokenInfo' _ _) -> if tokenInfo == tokenInfo' then Just () else Nothing)

--tokenPosition
--tokenPostionString



-- A.1 Source text

-- A.1.1 Library source text

-- A.1.2 Configuration source text

-- A.1.3 Module and primitive source text

source_text = do m <- many description
                 eof
                 return m

description = module_declaration -- <|> udp_declaration

module_nonansi_header
  =   do attribute_instance_list
         tok_ KW_module
         identifier
         parameter_port_list
         list_of_ports
         tok_ Sym_semi

module_ansi_header
  =   do attribute_instance_list
         tok_ KW_module
         identifier
         parameter_port_list
         list_of_port_declarations
         tok_ Sym_semi
  
module_declaration
  =   do module_nonansi_header
         many module_item
         tok_ KW_endmodule
         optional endlabel
  <|> do module_ansi_header
         many non_port_module_item
         tok_ KW_endmodule
         optional endlabel

{-
//| attribute_instance_list Kw_module identifier Sym_paren_l_dot_aster_paren_r Sym_semi module_item_list Kw_endmodule label_option
//  { check_labels $3 $8; Module_declaration $3 }
//| extern module_nonansi_header
//| extern module_ansi_header
-}



-- A.1.4 Module parameters and ports

parameter_port_list
  =   do return []
  <|> do tok_ Sym_pound
         tok_ Sym_paren_l
         p <- parameter_declaration_list
         tok_ Sym_paren_r
         return p

{-

list_of_ports
  : Sym_paren_l port_list Sym_paren_r    { rev $2 }
  ;

list_of_port_declarations
  :                                                  { [] }
  | Sym_paren_l                       Sym_paren_r    { [] }
  | Sym_paren_l port_declaration_list Sym_paren_r    { rev (snd $2) }
  ;

port
  : port_expression                                               { $1 }
//| Sym_dot identifier Sym_paren_l                 Sym_paren_r    { () }
//| Sym_dot identifier Sym_paren_l port_expression Sym_paren_r    { () }
  ;

port_list
  :                     port    { [$1] }
  | port_list Sym_comma port    { $3 :: $1 }
  ;

port_expression
  : port_reference                                 { $1 }
//| Sym_brace_l port_reference_list Sym_brace_r    { () }
  ;

port_reference
  : identifier                                             { $1 }
//| identifier Sym_brack_l range_expression Sym_brack_r    { () }
  ;

//port_reference_list
//  :                               port_reference    { () }
//  | port_reference_list Sym_comma port_reference    { () }
//  ;

port_declaration
  : attribute_instance_list inout_declaration     { $2 }
  | attribute_instance_list input_declaration     { $2 }
  | attribute_instance_list output_declaration    { $2 }
  ;

port_declaration_list
  :                                 port_direction ansi_port_declaration   { let dt, id = $2 in $1,      [id, Module_item_port_declaration ($1,     dt, [id])] }
  | port_declaration_list Sym_comma port_direction ansi_port_declaration   { let dt, id = $4 in $3,     ((id, Module_item_port_declaration ($3,     dt, [id])) :: snd $1) }
  | port_declaration_list Sym_comma                ansi_port_declaration   { let dt, id = $3 in fst $1, ((id, Module_item_port_declaration (fst $1, dt, [id])) :: snd $1) }
  ;

port_direction
  : Kw_input   { Port_direction_input  (snd $1) }
  | Kw_output  { Port_direction_output (snd $1) }
  | Kw_inout   { Port_direction_inout  (snd $1) }
//| Kw_ref     { () }
  ;
  
net_port_header
  : port_type   { $1 }
  ;

variable_port_header
  : data_type   { () }
  ;
  
//interface_port_header
//  : identifier                      { () }
//  | identifier Sym_dot identifier   { () }
//  : Kw_interface                    { () }
//  | Kw_interface Sym_dot identifier { () }
//  ;

ansi_port_declaration
//:                       identifier unpacked_dimension_list                             { () }
//: net_port_header       identifier unpacked_dimension_list                             { $1, $2, $3 }
  : net_port_header       identifier                                                     { $1, $2 }
//|                       identifier variable_dimension expression_default               { () }
//| variable_port_header  identifier variable_dimension expression_default               { () }
//| interface_port_header identifier unpacked_dimension_list                             { () }
//|                       Sym_dot identifier Sym_paren_l expression_option Sym_paren_r   { () }
//| net_port_header       Sym_dot identifier Sym_paren_l expression_option Sym_paren_r   { () }
//| variable_port_header  Sym_dot identifier Sym_paren_l expression_option Sym_paren_r   { () }
  ;

// (*A.1.5 Module items*)

module_common_item
  : module_or_generate_item_declaration    { $1 }
//| interface_instantiation                { () }
//| program_instantiation                  { () }
//| concurrent_assertion_item              { () }
//| bind_directive                         { () }
  | continuous_assign                      { $1 }
//| net_alias                              { () }
  | initial_construct                      { $1 }
//| final_construct                        { () }
  | always_construct                       { $1 }
  ;

-}

module_item = port_declaration <|> non_port_module_item

{-

module_or_generate_item
//: attribute_instance_list parameter_override    { () }
  : attribute_instance_list gate_instantiation    { $2 }
//| attribute_instance_list udp_instantiation     { () }
  | attribute_instance_list module_instantiation  { $2 }
  | attribute_instance_list module_common_item    { $2 }
  ;

module_or_generate_item_declaration
  : package_or_generate_item_declaration             { $1 }
  | genvar_declaration                               { $1 }
//| clocking_declaration                             { () }
//| default clocking clocking_identifier Sym_semi    { () }
  ;

non_port_module_item
  : generated_module_instantiation                   { $1 }
  | module_or_generate_item                          { $1 }
//| specify_block                                    { () }
//| attribute_instance_list specparam_declaration    { () }
//| program_declaration                              { () }
  | module_declaration                               { Module_item_module_declaration $1 }
//| timeunits_declaration                            { () }
  ;

//parameter_override
//  : Kw_defparam list_of_defparam_assignments Sym_semi    { () }
//  ;

//bind_directive
//  : Kw_bind hierarchical_identifier constant_select bind_instantiation Sym_semi    { () }
//  ;

//bind_instantiation
//  : program_instantiation    { () }
//  | module_instantiation     { () }
//  | interface_instantiation  { () }
//  ;

// (*A.1.6 Interface items*)

// (*A.1.7 Program items*)

// (*A.1.8 Class items*)

// (*A.1.9 Constraints*)

// (*A.1.10 Package items*)

//package_item
//: package_or_generate_item_declaration   { () }
//| specparam_declaration                  { () }
//| anonymous_program                      { () }
//| timeunits_declaration                  { () }
//;

package_or_generate_item_declaration
  : net_declaration                          { $1 }
  | data_declaration                         { Module_item_data_declaration }
  | task_declaration                         { Module_item_task_declaration }
  | function_declaration                     { Module_item_function_declaration }
//| dpi_import_export                        { () }
//| extern_constraint_declaration            { () }
//| class_declaration                        { () }
//| class_constructor_declaration            { () }
  | parameter_declaration                    { $1 }
  | local_parameter_declaration              { $1 }
//| covergroup_declaration                   { () }
//| overload_declaration                     { () }
//| concurrent_assertion_item_declaration    { () }
  | Sym_semi                                 { Module_item_null }
  ;

//anonymous_program
//  : program Sym_semi anonymous_program_item_list endprogram
//  ;
//
//anonymous_program_item
//  : task_declaration
//  | function_declaration
//  | class_declaration
//  | covergroup_declaration
//  | class_constructor_declaration
//  | Sym_semi
//  ;
//
//anonymous_program_item_list
//  :
//  | anonymous_program_item_list anonymous_program_item
//  ;

// (*A.2 Declarations*)

// (*A.2.1 Declaration types*)

// (*A.2.1.1 Module parameter declarations*)

local_parameter_declaration
  : Kw_localparam data_type_or_implicit list_of_param_assignments Sym_semi  { Module_item_local_parameter_declaration ($2, $3) }
  ;

parameter_declaration
  : Kw_parameter data_type_or_implicit list_of_param_assignments Sym_semi   { Module_item_parameter_declaration ($2, $3) }
  ;

parameter_declaration_list  // (* XXX Clean up. *)
  :                                      Kw_parameter data_type_or_implicit param_assignment  { $2,     [Module_item_parameter_declaration ($2, [fst $3, snd $3])] }
  | parameter_declaration_list Sym_comma Kw_parameter data_type_or_implicit param_assignment  { $4,     (Module_item_parameter_declaration ($4, [fst $5, snd $5]) :: snd $1) }
  | parameter_declaration_list Sym_comma                                    param_assignment  { fst $1, (Module_item_parameter_declaration (fst $1, [fst $3, snd $3]) :: snd $1) }
  ;

specparam_declaration
  : Kw_specparam                  list_of_specparam_assignments Sym_semi    { () }
  | Kw_specparam packed_dimension list_of_specparam_assignments Sym_semi    { () }
  ;

// (*A.2.1.2 Port declarations*)

inout_declaration
  : Kw_inout port_type list_of_port_identifiers Sym_semi        { Module_item_port_declaration (Port_direction_inout (snd $1), $2, $3)  }
  ;

input_declaration
  : Kw_input port_type list_of_port_identifiers     Sym_semi    { Module_item_port_declaration (Port_direction_input (snd $1), $2, $3)  }
//| Kw_input data_type list_of_variable_identifiers Sym_semi    { () }
  ;

output_declaration
  : Kw_output port_type list_of_port_identifiers     Sym_semi   { Module_item_port_declaration (Port_direction_output (snd $1), $2, $3)  }
//| Kw_output data_type list_of_variable_identifiers Sym_semi   { () }
  ;

// (*A.2.1.3 Type declarations*)

data_declaration
  :          variable_declaration  { () }
  | Kw_const variable_declaration  { () }
  | type_declaration               { () }
  | package_import_declaration     { () }
//| virtual_interface_declaration  { () }
  ;

package_import_declaration
  : Kw_import package_import_item_list Sym_semi  { () }
  ;

package_import_item
  : identifier Sym_colon_colon identifier  { () }
  | identifier Sym_colon_colon Sym_aster   { () }
  ;

package_import_item_list
  :                                    package_import_item  { () }
  | package_import_item_list Sym_colon package_import_item  { () }
  ;

genvar_declaration
  : Kw_genvar list_of_genvar_identifiers Sym_semi    { Module_item_genvar_declaration (rev $2) }
  ;

net_declaration
  : net_type_or_trireg drive_strength_option             signing_option packed_dimension_list delay3_option list_of_net_decl_assignments Sym_semi    { Module_item_net_declaration (Data_type_net ($3, $4), $6) }
//| net_type_or_trireg drive_strength_option Kw_vectored signing_option packed_dimension_list delay3_option list_of_net_decl_assignments Sym_semi    { () }
//| net_type_or_trireg drive_strength_option Kw_scalared signing_option packed_dimension_list delay3_option list_of_net_decl_assignments Sym_semi    { () }
  ;

type_declaration
  : Kw_typedef data_type identifier variable_dimension Sym_semi                      { () }
//| Kw_typedef interface_instance_identifier Sym_dot identifier identifier Sym_semi  { () }
  | Kw_typedef           identifier Sym_semi   { () }
  | Kw_typedef Kw_enum   identifier Sym_semi   { () }
  | Kw_typedef Kw_struct identifier Sym_semi   { () }
  | Kw_typedef Kw_union  identifier Sym_semi   { () }
  | Kw_typedef Kw_class  identifier Sym_semi   { () }
  ;

variable_declaration
  : data_type list_of_variable_decl_assignments Sym_semi  { () }
  ;

lifetime
  : Kw_static     { () }
  | Kw_automatic  { () }
  ;

lifetime_option
  :           { () }
  | lifetime  { () }
  ;

// (*A.2.2 Declaration data types*)

// (*A.2.2.1 Net and variable types*)

//casting_type
//  : simple_type
//  | size
//  | signing_option
//  ;

data_type
  : integer_vector_type signing_option packed_dimension_list   { Data_type }
  | integer_atom_type signing_option                           { Data_type }
  | non_integer_type                                           { Data_type }
  | struct_union                Sym_brace_l struct_union_member_list Sym_brace_r packed_dimension_list   { Data_type }
//| struct_union packed signing_option Sym_brace_l struct_union_member_list Sym_brace_r packed_dimension_list
  | Kw_enum                Sym_brace_l enum_name_declaration_list Sym_brace_r   { Data_type }
  | Kw_enum enum_base_type Sym_brace_l enum_name_declaration_list Sym_brace_r   { Data_type }
//| Kw_string
//| Kw_chandle
//| Kw_virtual [ interface ] interface_identifier
//| [ class_scope | package_scope ] type_identifier { packed_dimension }
//| class_type
  | Kw_event                          { Data_type }
//| ps_covergroup_identifier
  ;

data_type_or_implicit
  : data_type                              { $1 }
  | signing_option packed_dimension_list   { Data_type }
  ;

enum_base_type
  : integer_atom_type signing_option   { () }
  | integer_vector_type signing_option   { () }
  | integer_vector_type signing_option packed_dimension   { () }
  | identifier   { () }
  | identifier packed_dimension   { () }
  ;

enum_name_declaration
  : identifier                                                                   expression_default   { () }
  | identifier Sym_brack_l integral_number                           Sym_brack_r expression_default   { () }
  | identifier Sym_brack_l integral_number Sym_colon integral_number Sym_brack_r expression_default   { () }
  ;

enum_name_declaration_list
  :                                      enum_name_declaration  { [$1] }
  | enum_name_declaration_list Sym_comma enum_name_declaration  { $3 :: $1 }
  ;

//class_scope
//  : class_type Sym_colon_colon  { () }
//  ;

//class_type
//  : ps_class_identifier [ parameter_value_assignment ] { Sym_colon_colon class_identifier [ parameter_value_assignment ] }
//  ;

integer_type
  : integer_vector_type  { () }
  | integer_atom_type    { () }
  ;

integer_atom_type
  : Kw_byte       { () }
  | Kw_shortint   { () }
  | Kw_int        { () }
  | Kw_longint    { () }
  | Kw_integer    { () }
  | Kw_time       { () }
  ;

integer_vector_type
  : Kw_bit        { () }
  | Kw_logic      { () }
  | Kw_reg        { () }
  ;

non_integer_type
  : Kw_shortreal  { () }
  | Kw_real       { () }
  | Kw_realtime   { () }
  ;

net_type
//: Kw_supply0    { () }
//| Kw_supply1    { () }
//| Kw_tri        { () }
//| Kw_triand     { () }
//| Kw_trior      { () }
//| Kw_tri0       { () }
//| Kw_tri1       { () }
  : Kw_wire       { () }
//| Kw_wand       { () }
//| Kw_wor        { () }
  ;

port_type
  :                    signing_option packed_dimension_list  { Data_type_net ($1, $2) }
  | net_type_or_trireg signing_option packed_dimension_list  { Data_type_net ($2, $3) }
  ;

net_type_or_trireg
  : net_type   { () }
//| Kw_trireg  { () }
  ;

signing
  : Kw_signed    { Signed }
  | Kw_unsigned  { Unsigned }
  ;

signing_option
  :                { Unsigned }
  | signing        { $1 }
  ;

simple_type
  : integer_type        { () }
  | non_integer_type    { () }
  | identifier          { () }
  ;

struct_union_member
  : attribute_instance_list data_type_or_void list_of_variable_identifiers Sym_semi  { () }
  ;

struct_union_member_list
  :                          struct_union_member   { [$1] }
  | struct_union_member_list struct_union_member   { $2 :: $1 }
  ;

data_type_or_void
  : data_type  { () }
  | Kw_void    { () }
  ;

struct_union
  : Kw_struct           { () }
  | Kw_union            { () }
  | Kw_union Kw_tagged  { () }
  ;

// (*A.2.2.2 Strengths*)

drive_strength_option
  :    { () }
  ;

// (*A.2.2.3 Delays*)

delay3
  : Sym_pound delay_value                                                                        { () }
  | Sym_pound Sym_paren_l delay_value Sym_paren_r                                                { () }
  | Sym_pound Sym_paren_l delay_value Sym_comma delay_value Sym_paren_r                          { () }
  | Sym_pound Sym_paren_l delay_value Sym_comma delay_value Sym_comma delay_value Sym_paren_r    { () }
  ;

delay3_option
  :           { () }
  | delay3    { () }
  ;

delay2
  : Sym_pound delay_value                                                                        { () }
  | Sym_pound Sym_paren_l delay_value Sym_paren_r                                                { () }
  | Sym_pound Sym_paren_l delay_value Sym_comma delay_value Sym_paren_r                          { () }
  ;

delay2_option
  :           { () }
  | delay2    { () }
  ;

delay_value
  : unsigned_number         { () }
  | identifier              { () }
//  | parameter_identifier    { () }
//  | specparam_identifier    { () }
//(*XXX*)  | mintypmax_expression    { () }
  ;

// (*A.2.3 Declaration lists*)

list_of_genvar_identifiers
  :                                      identifier    { [$1] }
  | list_of_genvar_identifiers Sym_comma identifier    { $3 :: $1 }
  ;

list_of_net_decl_assignments
  : list_of_net_decl_assignments_0    { rev $1 }
  ;

list_of_net_decl_assignments_0
  :                                          net_decl_assignment    { [$1] }
  | list_of_net_decl_assignments_0 Sym_comma net_decl_assignment    { $3 :: $1 }
  ;

list_of_param_assignments
  : list_of_param_assignments_0    { rev $1 }
  ;

list_of_param_assignments_0
  :                                       param_assignment    { [$1] }
  | list_of_param_assignments_0 Sym_comma param_assignment    { $3 :: $1 }
  ;

list_of_port_identifiers
  : list_of_port_identifiers_0    { rev $1 }
  ;

list_of_port_identifiers_0
  :                                      identifier    { [$1] }
  | list_of_port_identifiers_0 Sym_comma identifier    { $3 :: $1 }
  ;

list_of_specparam_assignments
  : list_of_specparam_assignments_0    { () }
  ;

list_of_specparam_assignments_0
  :                                           specparam_assignment    { () }
  | list_of_specparam_assignments_0 Sym_comma specparam_assignment    { () }
  ;

list_of_tf_variable_identifiers
  :                                           identifier variable_dimension expression_default    { () }
  | list_of_tf_variable_identifiers Sym_comma identifier variable_dimension expression_default    { () }
  ;

list_of_variable_decl_assignments
  :                                             variable_decl_assignment  { () }
  | list_of_variable_decl_assignments Sym_comma variable_decl_assignment  { () }
  ;

list_of_variable_identifiers
  : list_of_variable_identifiers_0    { () }
  ;

list_of_variable_identifiers_0
  :                                          identifier variable_dimension    { () }
  | list_of_variable_identifiers_0 Sym_comma identifier variable_dimension    { () }
  ;

list_of_variable_port_identifiers
  :                                             identifier variable_dimension expression_default    { () }
  | list_of_variable_port_identifiers Sym_comma identifier variable_dimension expression_default    { () }
  ;

// (*A.2.4 Declaration assignments*)

net_decl_assignment
  : identifier expression_default    { $1, $2 }
  ;

param_assignment
  : identifier Sym_eq expression    { $1, $3 }
  ;

specparam_assignment
  : identifier Sym_eq mintypmax_expression    { () }
  ;

variable_decl_assignment
  : identifier variable_dimension expression_default  { () }
//| dynamic_array_variable_identifier Sym_brack_l Sym_brack_r expression_default
//| class_variable_identifier
//| class_variable_identifier Sym_eq class_new
//| [ covergroup_variable_identifier ] = new [ ( list_of_arguments ) ]
  ;

//debug1: { Report.debug "debug1" } ;
//debug2: { Report.debug "debug2" } ;

// (*A.2.5 Declaration ranges*)

unpacked_dimension
  : range                                { () }
  | Sym_brack_l expression Sym_brack_r   { () }
  ;
  
unpacked_dimension_list
  : unpacked_dimension_list_0  { rev $1 }
  ;

unpacked_dimension_list_0
  :                                               { [] }
  | unpacked_dimension_list_0 unpacked_dimension  { $2 :: $1 }
  ;

packed_dimension
  : range              { Packed_dimension_range (fst $1, snd $1) }
  | unsized_dimension  { Packed_dimension_unsized }
  ;

packed_dimension_list
  : packed_dimension_list_0  { rev $1 }
  ;

packed_dimension_list_0
  :                                            { [] }
  | packed_dimension_list_0 packed_dimension   { $2 :: $1 }
  ;
  
associative_dimension
  : Sym_brack_l data_type Sym_brack_r   { () }
  | Sym_brack_l Sym_aster Sym_brack_r   { () }
  ;
  
variable_dimension
  :                                   { () }
  | sized_or_unsized_dimension_list   { () }
  | associative_dimension             { () }
  | queue_dimension                   { () }
  ;

queue_dimension
  : Sym_brack_l Sym_dollar                      Sym_brack_r   { () }
  | Sym_brack_l Sym_dollar Sym_colon expression Sym_brack_r   { () }
  ;
  
unsized_dimension
  : Sym_brack_l Sym_brack_r  { () }
  ;
  
sized_or_unsized_dimension
  : unpacked_dimension  { () }
  | unsized_dimension   { () }
  ;

sized_or_unsized_dimension_list
  :                                 sized_or_unsized_dimension   { () }
  | sized_or_unsized_dimension_list sized_or_unsized_dimension   { () }
  ;


// (*2001*)

range
  : Sym_brack_l msb_constant_expression Sym_colon lsb_constant_expression Sym_brack_r    { $2, $4 }
  ;

range_option
  :          { () }
  | range    { () }
  ;

// (*A.2.6 Function declarations*)

function_data_type
  : data_type  { () }
  | Kw_void    { () }
  ;

function_data_type_or_implicit
  : function_data_type                    { () }
  | signing_option packed_dimension_list  { () }
  ;

function_declaration
  : Kw_function function_body_declaration  { () }
  ;

function_body_declaration
  : function_data_type_or_implicit identifier                                             Sym_semi tf_item_declaration_list    statement_or_null_list Kw_endfunction label_option  { check_labels $2 $7; () }
  | function_data_type_or_implicit identifier Sym_paren_l tf_port_list_option Sym_paren_r Sym_semi block_item_declaration_list statement_or_null_list Kw_endfunction label_option  { check_labels $2 $10; () }
  ;

label_option
  :                        { Label_none }
  | Sym_colon identifier   { Label $2 }
  ;
-}

endlabel = do tok_ Sym_colon
              identifier

{-

// (*A.2.7 Task declarations*)

task_declaration
  : Kw_task task_body_declaration  { $1 }
  ;

task_body_declaration
  : identifier                                             Sym_semi tf_item_declaration_list    statement_or_null_list Kw_endtask label_option  { check_labels $1 $6; () }
  | identifier Sym_paren_l tf_port_list_option Sym_paren_r Sym_semi block_item_declaration_list statement_or_null_list Kw_endtask label_option  { check_labels $1 $9; () }
  ;

tf_item_declaration
  : block_item_declaration  { () }
  | tf_port_declaration     { () }
  ;

tf_item_declaration_list
  :                                                { () }
  | tf_item_declaration_list tf_item_declaration   { () }
  ;

tf_port_list
  :                        tf_port_item  { () }
  | tf_port_list Sym_comma tf_port_item  { () }
  ;

tf_port_list_option
  :               { () }
  | tf_port_list  { () }
  ;

tf_port_item
  : attribute_instance_list                   data_type_or_implicit identifier variable_dimension expression_default  { () }
  | attribute_instance_list tf_port_direction data_type_or_implicit identifier variable_dimension expression_default  { () }
  ;

tf_port_direction
  : port_direction   { () }
  | Kw_const Kw_ref  { () }
  ;

tf_port_declaration
  : attribute_instance_list tf_port_direction data_type_or_implicit list_of_tf_variable_identifiers Sym_semi  { () }
  ;

// (*A.2.8 Block item declarations*)

block_item_declaration
  : attribute_instance_list data_declaration               { () }
  | attribute_instance_list local_parameter_declaration    { () }
  | attribute_instance_list parameter_declaration          { () }
  ;

block_item_declaration_list
  : block_item_declaration_list_0    { () }
  ;

block_item_declaration_list_0
  :                                                         { () }
  | block_item_declaration_list_0 block_item_declaration    { () }
  ;

// (*A.2.9 Interface declarations*)

// (*A.2.10 Assertion declarations*)

/*
concurrent_assertion_item
  :                      concurrent_assertion_statement     { () }
  | identifier Sym_colon concurrent_assertion_statement     { () }
  ;

concurrent_assertion_statement
  : assert_property_statement    { () }
  | assume_property_statement    { () }
  | cover_property_statement     { () }
  ;
  
assert_property_statement
  : Kw_assert Kw_property Sym_paren_l property_spec Sym_paren_l action_block    { () }
  ;
  
assume_property_statement
  : Kw_assume Kw_property Sym_paren_l property_spec Sym_paren_r Sym_semi    { () }
  ;
  
cover_property_statement
  : Kw_cover Kw_property Sym_paren_l property_spec Sym_paren_r statement_or_null     { () }
  ;

expect_property_statement
  : Kw_expect Sym_paren_l property_spec Sym_paren_r action_block     { () }
  ;

property_instance
  : identifier                                            { () }
  | identifier Sym_paren_l                 Sym_paren_r    { () }
  | identifier Sym_paren_l actual_arg_list Sym_paren_r    { () }
  ;

concurrent_assertion_item_declaration
  : property_declaration     { () }
  | sequence_declaration     { () }
  ;

property_declaration
  : Kw_property identifier list_of_formals_option Sym_semi assertion_variable_declaration_list property_spec Sym_semi Kw_endproperty label_option
    { check_labels $2 $9; () }
  ;

property_spec
  :                                                                             property_expr     { () }
  | clocking_event                                                              property_expr     { () }
  | Kw_disable Kw_iff Sym_paren_l expression_or_dist Sym_paren_r                property_expr     { () }
  | clocking_event Kw_disable Kw_iff Sym_paren_l expression_or_dist Sym_paren_r property_expr     { () }
  ;

property_expr
  : sequence_expr                                                                           { () }
  | Sym_paren_l property_expr Sym_paren_r                                                   { () }
  | Kw_not property_expr                                                                    { () }
//(*XXX*)| property_expr Kw_or property_expr                                                       { () }
//(*XXX*)| property_expr Kw_and property_expr                                                      { () }
  | sequence_expr Sym_bar_dash_gt property_expr                                             { () }
  | sequence_expr Sym_bar_eq_gt property_expr                                               { () }
  | Kw_if Sym_paren_l expression_or_dist Sym_paren_r property_expr %prec Dangle_else        { () }
  | Kw_if Sym_paren_l expression_or_dist Sym_paren_r property_expr Kw_else property_expr    { () }
//(*XXX*)| property_instance                                                                       { () }
  | clocking_event property_expr                                                            { () }
  ;

sequence_declaration
  : Kw_sequence identifier list_of_formals_option Sym_semi assertion_variable_declaration_list sequence_expr Sym_semi Kw_endsequence label_option    { () }
    { check_labels $2 $9; () }
  ;

sequence_expr
  :               cycle_delay_range sequence_expr cycle_delay_range_sequence_expr_list    { () }
  | sequence_expr cycle_delay_range sequence_expr cycle_delay_range_sequence_expr_list    { () }
  | expression_or_dist                                                                    { () }
  | expression_or_dist boolean_abbrev                                                     { () }
//| Sym_paren_l expression_or_dist sequence_match_item_list Sym_paren_r                   { () }
//| Sym_paren_l expression_or_dist sequence_match_item_list Sym_paren_r boolean_abbrev    { () }
//| sequence_instance                                                                     { () }
//| sequence_instance sequence_abbrev                                                     { () }
//| Sym_paren_l sequence_expr sequence_match_item_list Sym_paren_r                        { () }
//| Sym_paren_l sequence_expr sequence_match_item_list Sym_paren_r sequence_abbrev        { () }
  | sequence_expr Kw_and sequence_expr                                                    { () }
  | sequence_expr Kw_intersect sequence_expr                                              { () }
  | sequence_expr Kw_or sequence_expr                                                     { () }
//| Kw_first_match Sym_paren_l sequence_expr sequence_match_item_list Sym_paren_r         { () }
  | expression_or_dist Kw_throughout sequence_expr                                        { () }
  | sequence_expr Kw_within sequence_expr                                                 { () }
  | clocking_event sequence_expr                                                          { () }
  ;

cycle_delay_range
  : Sym_pound_pound integral_number                                               { () }
  | Sym_pound_pound identifier                                                    { () }
  | Sym_pound_pound Sym_paren_l expression Sym_paren_r                   { () }
  | Sym_pound_pound Sym_brack_l cycle_delay_const_range_expression Sym_brack_r    { () }
  ;

cycle_delay_range_sequence_expr_list
  :                                                                         { () }
  | cycle_delay_range_sequence_expr_list cycle_delay_range sequence_expr    { () }
  ;

 sequence_method_call
   : sequence_instance Sym_dot identifier     { () }
   ;

//sequence_match_item
//  : operator_assignment      { () }
//  | inc_or_dec_expression    { () }
//  | subroutine_call          { () }
//  ;
//
//sequence_match_item_list
//  :                                                           { () }
//  | sequence_match_item_list Sym_comma sequence_match_item    { () }
//  ;

 sequence_instance
   : identifier                                            { () }
   | identifier Sym_paren_l                 Sym_paren_r    { () }
   | identifier Sym_paren_l actual_arg_list Sym_paren_r    { () }
   ;

formal_list_item
  : identifier                           { () }
  | identifier Sym_eq actual_arg_expr    { () }
  ;

list_of_formals
  :                           formal_list_item    { () }
  | list_of_formals Sym_comma formal_list_item    { () }
  ;

list_of_formals_option
  :                                            { () }
  | Sym_paren_l                 Sym_paren_r    { () }
  | Sym_paren_l list_of_formals Sym_paren_r    { () }
  ;

actual_arg_list
  : actual_arg_expr_list                { () }
  | identifier_actual_arg_expr_list     { () }
  ;

identifier_actual_arg_expr_list
  :                                           Sym_dot identifier Sym_paren_l actual_arg_expr Sym_paren_r    { () }
  | identifier_actual_arg_expr_list Sym_comma Sym_dot identifier Sym_paren_l actual_arg_expr Sym_paren_r    { () }
  ;

actual_arg_expr
  : event_expression    { () }
  | Sym_dollar          { () }
  ;

actual_arg_expr_list
  :                                actual_arg_expr    { () }
  | actual_arg_expr_list Sym_comma actual_arg_expr    { () }
  ;

boolean_abbrev
  : consecutive_repetition        { () }
  | non_consecutive_repetition    { () }
  | goto_repetition               { () }
  ;

sequence_abbrev
  : consecutive_repetition     { () }
  ;

consecutive_repetition
  : Sym_brack_l_aster const_or_range_expression Sym_brack_r    { () }
  ;

non_consecutive_repetition
  : Sym_brack_l_eq const_or_range_expression Sym_brack_r    { () }
  ;

goto_repetition
  : Sym_brack_l_dash_gt const_or_range_expression Sym_brack_r    { () }
  ;

const_or_range_expression
  : expression                    { () }
  | cycle_delay_const_range_expression     { () }
  ;

cycle_delay_const_range_expression
  : expression Sym_colon expression    { () }
  | expression Sym_colon Sym_dollar             { () }
  ;

expression_or_dist
  : expression                                              { () }
//| expression Kw_dist Sym_brace_l dist_list Sym_brace_r    { () }
  ;

assertion_variable_declaration
  : data_type list_of_variable_identifiers Sym_semi     { () }
  ;

assertion_variable_declaration_list
  :                                                                       { () }
  | assertion_variable_declaration_list assertion_variable_declaration    { () }
  ;
*/

// (*A.2.11 Covergroup declarations*)

// (*A.3 Primitive instances*)

// (*A.3.1 Primitive instantiation and instances*)

gate_instantiation
  : n_input_gatetype  drive_strength_option delay2_option n_input_gate_instance_list  Sym_semi  { $1 (rev $4) }
  | n_output_gatetype drive_strength_option delay2_option n_output_gate_instance_list Sym_semi  { $1 (rev $4) }
  ;

n_input_gate_instance
  :                  Sym_paren_l expression_list Sym_paren_r  { Label_none, $2 }
  | name_of_instance Sym_paren_l expression_list Sym_paren_r  { Label $1, $3 }
  ;

n_input_gate_instance_list
  :                                      n_input_gate_instance  { [$1] }
  | n_input_gate_instance_list Sym_comma n_input_gate_instance  { $3 :: $1 }
  ;

n_output_gate_instance
  :                  Sym_paren_l expression_list Sym_paren_r  { Label_none, $2 }
  | name_of_instance Sym_paren_l expression_list Sym_paren_r  { Label $1, $3 }
  ;

n_output_gate_instance_list
  :                                       n_output_gate_instance  { [$1] }
  | n_output_gate_instance_list Sym_comma n_output_gate_instance  { $3 :: $1 }
  ;

n_input_gatetype
  : Kw_and    { (fun stuff -> Module_item_gate_instance_and  stuff) }
  | Kw_nand   { (fun stuff -> Module_item_gate_instance_nand stuff) }
  | Kw_or     { (fun stuff -> Module_item_gate_instance_or   stuff) }
  | Kw_nor    { (fun stuff -> Module_item_gate_instance_nor  stuff) }
  | Kw_xor    { (fun stuff -> Module_item_gate_instance_xor  stuff) }
  | Kw_xnor   { (fun stuff -> Module_item_gate_instance_xnor stuff) }
  ;

n_output_gatetype
  : Kw_buf    { (fun stuff -> Module_item_gate_instance_buf stuff) }
  | Kw_not    { (fun stuff -> Module_item_gate_instance_not stuff) }
  ;

// (*A.3.2 Primitive strengths*)

// (*A.3.3 Primitive terminals*)

// (*A.3.4 Primitive gate and switch types*)

// (*A.4 Module and generated instantiation*)

// (*A.4.1 Module instantiation*)

module_instantiation
  : identifier                            module_instance_list Sym_semi    { Module_item_module_instantiation ($1, Connection_list_ordered [], rev $2) }
  | identifier parameter_value_assignment module_instance_list Sym_semi    { Module_item_module_instantiation ($1, $2, rev $3) }
  ;

parameter_value_assignment
  : Sym_pound Sym_paren_l list_of_parameter_assignments Sym_paren_r    { $3 }
  ;

list_of_parameter_assignments
  : ordered_parameter_assignment_list    { Connection_list_ordered (rev $1) }
  | named_parameter_assignment_list      { Connection_list_named (rev $1) }
  ;

ordered_parameter_assignment
  : expression    { Connection_ordered_expr $1 }
  ;

ordered_parameter_assignment_list
  :                                             ordered_parameter_assignment    { [$1] }
  | ordered_parameter_assignment_list Sym_comma ordered_parameter_assignment    { $3 :: $1 }
  ;

named_parameter_assignment
  : Sym_dot identifier Sym_paren_l            Sym_paren_r    { Connection_named_none $2 }
  | Sym_dot identifier Sym_paren_l expression Sym_paren_r    { Connection_named_expr ($2, $4) }
  ;

named_parameter_assignment_list
  :                                           named_parameter_assignment    { [$1] }
  | named_parameter_assignment_list Sym_comma named_parameter_assignment    { $3 :: $1 }
  ;

module_instance
  : name_of_instance Sym_paren_l list_of_port_connections Sym_paren_r    { $1, $3 }
  ;

module_instance_list
  :                                module_instance    { [$1] }
  | module_instance_list Sym_comma module_instance    { $3 :: $1 }
  ;

name_of_instance
  : identifier   { $1 }
  ;

list_of_port_connections
  : ordered_port_connection_list    { Connection_list_ordered (rev $1) }
  | named_port_connection_list      { Connection_list_named (rev $1) }
  ;

ordered_port_connection
  : attribute_instance_list               { Connection_ordered_none }
  | attribute_instance_list expression    { Connection_ordered_expr $2 }
  ;

ordered_port_connection_list
  :                                        ordered_port_connection    { [$1] }
  | ordered_port_connection_list Sym_comma ordered_port_connection    { $3 :: $1 }
  ;

named_port_connection
  : attribute_instance_list Sym_dot identifier                                       { Connection_named_same $3 }
  | attribute_instance_list Sym_dot identifier Sym_paren_l            Sym_paren_r    { Connection_named_none $3 }
  | attribute_instance_list Sym_dot identifier Sym_paren_l expression Sym_paren_r    { Connection_named_expr ($3, $5) }
//| attribute_instance_list Sym_dot_aster                                            { () }
  ;

named_port_connection_list
  :                                      named_port_connection    { [$1] }
  | named_port_connection_list Sym_comma named_port_connection    { $3 :: $1 }
  ;

// (*A.4.2 Generated instantiation*)

generated_module_instantiation
  : Kw_generate generate_module_item_list Kw_endgenerate    { Module_item_generate $2 }
  ;

generate_module_item
  : generate_module_conditional_statement        { $1 }
  | generate_module_case_statement               { $1 }
  | generate_module_loop_statement               { $1 }
  |                      generate_module_block   { $1 }
//| identifier Sym_colon generate_module_block   { () }
  | module_or_generate_item                      { Generate_module_item_module_item $1 }
  ;

generate_module_item_list
  : generate_module_item_list_0  { rev $1 }
  ;

generate_module_item_list_0
  :                                                     { [] }
  | generate_module_item_list_0 generate_module_item    { $2 :: $1 }
  ;

generate_module_conditional_statement
  : Kw_if Sym_paren_l expression Sym_paren_r generate_module_item %prec Dangle_else                { Generate_module_item_conditional_dangle ($3, $5) }
  | Kw_if Sym_paren_l expression Sym_paren_r generate_module_item Kw_else generate_module_item     { Generate_module_item_conditional ($3, $5, $7) }
  ;

generate_module_case_statement
  : Kw_case Sym_paren_l expression Sym_paren_r genvar_module_case_item_list Kw_endcase    { Generate_module_item_case ($3, $5) }
  ;

genvar_module_case_item
  : expression_list Sym_colon generate_module_item    { Genvar_module_case_item ($1, $3) }
  | Kw_default           generate_module_item         { Genvar_module_case_item_default $2 }
  | Kw_default Sym_colon generate_module_item         { Genvar_module_case_item_default $3 }
  ;

genvar_module_case_item_list
  :                              genvar_module_case_item    { [$1] }
  | genvar_module_case_item_list genvar_module_case_item    { $2 :: $1 }
  ;

generate_module_loop_statement
  : Kw_for Sym_paren_l genvar_decl_assignment Sym_semi expression Sym_semi genvar_assignment generate_module_named_block  { Generate_module_item_loop ($3, $5, $7, $8) }
  ;

genvar_assignment
  : identifier Sym_eq expression    { $1, $3 }
  ;

genvar_decl_assignment
  :           identifier Sym_eq expression  { Genvar_decl_assignment_existing ($1, $3) }
  | Kw_genvar identifier Sym_eq expression  { Genvar_decl_assignment_new ($2, $4) }
  ;

generate_module_named_block
  : Kw_begin Sym_colon identifier generate_module_item_list Kw_end label_option { check_labels $3 $6; Generate_module_item_block (Label $3, $4) }
  | identifier Sym_colon generate_module_block { match $3 with Generate_module_item_block (label, items) -> check_labels $1 label; Generate_module_item_block (Label $1, items) | _ -> raise (Invalid_argument "generate_module_named_block") }
  ;

generate_module_block
  : Kw_begin                      generate_module_item_list Kw_end              {                     Generate_module_item_block (Label_none, $2) }
  | Kw_begin Sym_colon identifier generate_module_item_list Kw_end label_option { check_labels $3 $6; Generate_module_item_block (Label $3, $4) }
  ;

// (*A.5 UDP declaration and instantiation*)

// (*A.5.1 UDP declaration*)

// (*A.5.2 UDP ports*)

// (*A.5.3 UDP body*)

// (*A.5.4 UDP instantiation*)

// (*A.6 Behavioral statements*)

// (*A.6.1 Continuous assignment statements*)

continuous_assign
  : Kw_assign delay3_option list_of_net_assignments      Sym_semi  { Module_item_continuous_assign (rev $3) }
//| Kw_assign               list_of_variable_assignments Sym_semi  { () }
//| Kw_assign delay_control list_of_variable_assignments Sym_semi  { () }
  ;
  
list_of_net_assignments
  :                                   net_assignment    { [$1] }
  | list_of_net_assignments Sym_comma net_assignment    { $3 :: $1 }
  ;
  
list_of_variable_assignments
  :                                        variable_assignment  { () }
  | list_of_variable_assignments Sym_comma variable_assignment  { () }
  ;
  
//net_alias
//  : Kw_alias lvalue Sym_eq lvalue net_alias_0 Sym_semi  { () }
//  ;

net_alias_0
  :                            { () }
  | net_alias_0 Sym_eq lvalue  { () }
  ;
  
net_assignment
  : lvalue Sym_eq expression  { $1, $3 }
  ;

// (*A.6.2 Procedural blocks and assignments*)

initial_construct
  : Kw_initial statement    { Module_item_initial $2 }
  ;

always_construct
  : Kw_always statement    { Module_item_always $2 }
  ;

blocking_assignment
  : lvalue Sym_eq                        expression    { Statement_blocking_assignment ($1, $3) }
  | lvalue Sym_eq delay_or_event_control expression    { Statement_blocking_assignment ($1, $4) }
  ;

nonblocking_assignment
  : lvalue Sym_lt_eq                        expression    { Statement_nonblocking_assignment ($1, $3) }
  | lvalue Sym_lt_eq delay_or_event_control expression    { Statement_nonblocking_assignment ($1, $4) }
  ;

//procedural_continuous_assignment
//: Kw_assign variable_assignment       { Statement_procedural_continuous_assignments (fst $2, snd $2) }
//| Kw_deassign variable_lvalue         { () }
//| Kw_force variable_assignment        { () }
//| Kw_force net_assignment             { () }
//| Kw_release variable_lvalue          { () }
//| Kw_release net_lvalue               { () }
  ;

variable_assignment
  : lvalue Sym_eq expression    { $1, $3 }
  ;

// (*A.6.3 Parallel and sequential blocks*)

//action_block
//  :                   statement_or_null     { () }
//  |           Kw_else statement_or_null     { () }
//  | statement Kw_else statement_or_null     { () }
//  ;

seq_block
  : Kw_begin                                                  statement_list Kw_end    { Statement_seq_block }
  | Kw_begin Sym_colon identifier block_item_declaration_list statement_list Kw_end    { Statement_seq_block }
  ;

// par_block
//   : Kw_fork [ Sym_colon block_identifier block_item_declaration_list ] statement_list Kw_join
//   ;

// (*A.6.4 Statements*)

statement_or_null
  : statement                         { $1 }
  | attribute_instance_list Sym_semi  { Statement_null }
  ;

statement
  :                      attribute_instance_list statement_item  { $2 }
//| identifier Sym_colon attribute_instance_list statement_item  { () }
  ;

statement_item
  : blocking_assignment Sym_semi                  { $1 }
  | nonblocking_assignment Sym_semi               { $1 }
//| procedural_continuous_assignment Sym_semi     { $1 }
  | case_statement                                { $1 }
  | conditional_statement                         { $1 }
  | subroutine_call_statement                     { $1 }
//| disable_statement                             { $1 }
  | event_trigger                                 { $1 }
  | loop_statement                                { $1 }
  | procedural_timing_control_statement           { $1 }
  | seq_block                                     { $1 }
//| wait_statement                                { $1 }
//| procedural_assertion_statement                { $1 }
  ;

statement_list
  : statement_list_0    { () }
  ;

statement_list_0
  :                               { () }
  | statement_list_0 statement    { () }
  ;

statement_or_null_list
  :                                           { () }
  | statement_or_null_list statement_or_null  { () }
  ;

// (*A.6.5 Timing control statements*)

delay_control
  : Sym_pound delay_value                                     { () }
  | Sym_pound Sym_paren_l mintypmax_expression Sym_paren_r    { () }
  ;

delay_or_event_control
  : delay_control                                                 { () }
  | event_control                                                 { () }
//(*XXX*)  | Kw_repeat Sym_paren_l expression Sym_paren_r event_control    { () }
  ;

//disable_statement
//  : Kw_disable hierarchical_identifier Sym_semi    { () }
//  ;

event_control
  : Sym_at identifier                                  { () }
  | Sym_at Sym_paren_l event_expression Sym_paren_r    { () }
  | Sym_at_aster                                       { () }
  | Sym_at Sym_paren_l_aster_paren_r                   { () }
  ;

event_trigger
  : Sym_dash_gt hierarchical_identifier Sym_semi    { Statement_event_trigger $2 }
  ;

event_expression
  : expression                                     { () }
//  | hierarchical_identifier                        { () }
  | Kw_posedge expression                          { () }
  | Kw_negedge expression                          { () }
  | event_expression Kw_or event_expression        { () }
  | event_expression Sym_comma event_expression    { () }
  ;

procedural_timing_control_statement
  : delay_or_event_control statement_or_null    { $2 }
  ;

//wait_statement
//  : Kw_wait Sym_paren_l expression Sym_paren_r statement_or_null    { () }
//  ;

// (*A.6.6 Conditional statements*)

conditional_statement
  : Kw_if Sym_paren_l expression Sym_paren_r statement_or_null %prec Dangle_else            { Statement_conditional_dangle ($3, $5) }
  | Kw_if Sym_paren_l expression Sym_paren_r statement_or_null Kw_else statement_or_null    { Statement_conditional ($3, $5, $7) }
  ;

// (*A.6.7 Case statements*)

case_statement
  : Kw_case  Sym_paren_l expression Sym_paren_r case_item_list Kw_endcase    { Statement_case  ($3, $5) }
  | Kw_casez Sym_paren_l expression Sym_paren_r case_item_list Kw_endcase    { Statement_casez ($3, $5) }
  | Kw_casex Sym_paren_l expression Sym_paren_r case_item_list Kw_endcase    { Statement_casex ($3, $5) }
  ;

case_item
  : expression_list Sym_colon statement_or_null    { Case_item ($1, $3) }
  | Kw_default           statement_or_null         { Case_item_default $2 }
  | Kw_default Sym_colon statement_or_null         { Case_item_default $3 }
  ;

case_item_list
  : case_item_list_0    { rev $1 }
  ;

case_item_list_0
  :                  case_item    { [$1] }
  | case_item_list_0 case_item    { $2 :: $1 }
  ;

// (*A.6.8 Looping statements*)

loop_statement
  : Kw_forever statement                                                                                             { Statement_loop_forever }
  | Kw_repeat Sym_paren_l expression Sym_paren_r statement                                                           { Statement_loop_repeat  }
  | Kw_while Sym_paren_l expression Sym_paren_r statement                                                            { Statement_loop_while   }
  | Kw_for Sym_paren_l variable_assignment Sym_semi expression Sym_semi variable_assignment Sym_paren_r statement    { Statement_loop_for     }
  ;

// (*A.6.9 Subroutine call statements*)

subroutine_call_statement
  : subroutine_call Sym_semi  { Statement_subroutine_call }
  ;

// (*A.6.10 Assertion statements*)

/*
procedural_assertion_statement
  : concurrent_assertion_statement    { () }
  | immediate_assert_statement        { () }
  ;

immediate_assert_statement
  : Kw_assert Sym_paren_l expression Sym_paren_r action_block     { () }
  ;
*/

// (*A.6.11 Clocking block*)

//clocking_event
//  : Sym_at identifier { () }
//  | Sym_at Sym_paren_l event_expression Sym_paren_r { () }
//  ;
//
//cycle_delay
//  : Sym_pound_pound integral_number                     { () }
//  | Sym_pound_pound identifier                          { () }
//  | Sym_pound_pound Sym_paren_l expression Sym_paren_r  { () }
//  ;

// (*A.7 Specify section*)

// (*A.7.1 Specify block declaration*)

// (*A.7.2 Specify path declarations*)

// (*A.7.3 Specify block terminals*)

// (*A.7.4 Specify path delays*)

// (*A.7.5 System timing checks*)

// (*A.7.5.1 System timing check commands*)

// (*A.7.5.2 System timing check command arguments*)

// (*A.7.5.3 System timing check event definitions*)

// (*A.8 Expressions*)

// (*A.8.1 Concatenations*)

concatenation
  : Sym_brace_l expression_list Sym_brace_r    { snd $1, $2 }
  ;

multiple_concatenation
  : Sym_brace_l expression concatenation Sym_brace_r    { Expression_multiple_concatenation (snd $1, $2, snd $3) }
  ;

// (*A.8.2 Subroutine calls*)

tf_call
  : hierarchical_identifier attribute_instance_list                                            { () }
  | hierarchical_identifier attribute_instance_list Sym_paren_l list_of_arguments Sym_paren_r  { () }
  ;

system_tf_call
  : system_identifier                                            { () }
  | system_identifier Sym_paren_l list_of_arguments Sym_paren_r  { () }
  ;

subroutine_call
  : tf_call         { () }
  | system_tf_call  { () }
//| method_call
//| randomize_call
  ;

function_subroutine_call
//: subroutine_call  { () }
  : hierarchical_identifier attribute_instance_list Sym_paren_l list_of_arguments Sym_paren_r  { () }
  | system_tf_call                                                                             { () }
  ;

list_of_arguments
  : expression_list  { () }
//: [ expression ] { , [ expression ] } { , . identifier ( [ expression ] ) }
//| . identifier ( [ expression ] ) { , . identifier ( [ expression ] ) }
  ;

//function_call
//  : hierarchical_identifier Sym_paren_l expression_list Sym_paren_r    { () }
//  ;
//
//system_function_call
//  : system_identifier                                          { () }
//  | system_identifier Sym_paren_l expression_list Sym_paren_r  { () }
//  ;

// (*A.8.3 Expressions*)

constant_range
  : expression Sym_colon expression  { $1, $3 }
  ;

part_select_range
  : constant_range  { $1 }
//| indexed_range   { () }
  ;

// (*2001*)

base_expression
  : expression    { () }
  ;

dimension_constant_expression
  : expression    { () }
  ;

expression
  : primary                                                          { $1 }
  | Sym_plus      attribute_instance_list primary %prec Unary        { Expression_unary_plus         (snd $1, $3) }
  | Sym_dash      attribute_instance_list primary %prec Unary        { Expression_unary_dash         (snd $1, $3) }
  | Sym_bang      attribute_instance_list primary                    { Expression_unary_bang         (snd $1, $3) }
  | Sym_tildy     attribute_instance_list primary                    { Expression_unary_tildy        (snd $1, $3) }
  | Sym_amp       attribute_instance_list primary                    { Expression_unary_amp          (snd $1, $3) }
  | Sym_tildy_amp attribute_instance_list primary                    { Expression_unary_tildy_amp    (snd $1, $3) }
  | Sym_bar       attribute_instance_list primary                    { Expression_unary_bar          (snd $1, $3) }
  | Sym_tildy_bar attribute_instance_list primary                    { Expression_unary_tildy_bar    (snd $1, $3) }
  | Sym_hat       attribute_instance_list primary                    { Expression_unary_hat          (snd $1, $3) }
  | Sym_tildy_hat attribute_instance_list primary                    { Expression_unary_tildy_hat    (snd $1, $3) }
  | Sym_hat_tildy attribute_instance_list primary                    { Expression_unary_hat_tildy    (snd $1, $3) }
  | expression Sym_plus        attribute_instance_list expression    { Expression_binary_plus        (snd $2, $1, $4) } 
  | expression Sym_dash        attribute_instance_list expression    { Expression_binary_dash        (snd $2, $1, $4) } 
  | expression Sym_aster       attribute_instance_list expression    { Expression_binary_aster       (snd $2, $1, $4) } 
  | expression Sym_slash       attribute_instance_list expression    { Expression_binary_slash       (snd $2, $1, $4) } 
  | expression Sym_percent     attribute_instance_list expression    { Expression_binary_percent     (snd $2, $1, $4) } 
  | expression Sym_eq_eq       attribute_instance_list expression    { Expression_binary_eq_eq       (snd $2, $1, $4) } 
  | expression Sym_bang_eq     attribute_instance_list expression    { Expression_binary_bang_eq     (snd $2, $1, $4) } 
  | expression Sym_eq_eq_eq    attribute_instance_list expression    { Expression_binary_eq_eq_eq    (snd $2, $1, $4) } 
  | expression Sym_bang_eq_eq  attribute_instance_list expression    { Expression_binary_bang_eq_eq  (snd $2, $1, $4) } 
  | expression Sym_amp_amp     attribute_instance_list expression    { Expression_binary_amp_amp     (snd $2, $1, $4) } 
  | expression Sym_bar_bar     attribute_instance_list expression    { Expression_binary_bar_bar     (snd $2, $1, $4) } 
  | expression Sym_aster_aster attribute_instance_list expression    { Expression_binary_aster_aster (snd $2, $1, $4) } 
  | expression Sym_lt          attribute_instance_list expression    { Expression_binary_lt          (snd $2, $1, $4) } 
  | expression Sym_lt_eq       attribute_instance_list expression    { Expression_binary_lt_eq       (snd $2, $1, $4) } 
  | expression Sym_gt          attribute_instance_list expression    { Expression_binary_gt          (snd $2, $1, $4) } 
  | expression Sym_gt_eq       attribute_instance_list expression    { Expression_binary_gt_eq       (snd $2, $1, $4) } 
  | expression Sym_amp         attribute_instance_list expression    { Expression_binary_amp         (snd $2, $1, $4) } 
  | expression Sym_bar         attribute_instance_list expression    { Expression_binary_bar         (snd $2, $1, $4) } 
  | expression Sym_hat         attribute_instance_list expression    { Expression_binary_hat         (snd $2, $1, $4) } 
  | expression Sym_hat_tildy   attribute_instance_list expression    { Expression_binary_hat_tildy   (snd $2, $1, $4) } 
  | expression Sym_tildy_hat   attribute_instance_list expression    { Expression_binary_tildy_hat   (snd $2, $1, $4) } 
  | expression Sym_gt_gt       attribute_instance_list expression    { Expression_binary_gt_gt       (snd $2, $1, $4) } 
  | expression Sym_lt_lt       attribute_instance_list expression    { Expression_binary_lt_lt       (snd $2, $1, $4) } 
  | expression Sym_gt_gt_gt    attribute_instance_list expression    { Expression_binary_gt_gt_gt    (snd $2, $1, $4) } 
  | expression Sym_lt_lt_lt    attribute_instance_list expression    { Expression_binary_lt_lt_lt    (snd $2, $1, $4) } 
  | expression Sym_question    attribute_instance_list expression Sym_colon expression    { Expression_conditional (snd $2, $1, $4, $6) }
  | Lit_string                                                       { Expression_string (snd $1, fst $1) }
  ;

expression_option
  :             { () }
  | expression  { () }
  ;

expression_default
  :                    { Expression_default_none }
  | Sym_eq expression  { Expression_default $2 }
  ;

expression_list
  : expression_list_0    { rev $1 }
  ;

expression_list_0
  :                             expression    { [$1] }
  | expression_list_0 Sym_comma expression    { $3 :: $1 }
  ;

lsb_constant_expression
  : expression    { $1 }
  ;

mintypmax_expression
  : expression                                              { $1 }
  | expression Sym_colon expression Sym_colon expression    { Expression_mintypmax (snd $2, $1, $3, $5) }
  ;

msb_constant_expression
  : expression    { $1 }
  ;

range_expression
  : expression                                                   { () }
  | msb_constant_expression Sym_colon lsb_constant_expression    { () }
  | base_expression Sym_plus_colon width_constant_expression     { () }
  | base_expression Sym_dash_colon width_constant_expression     { () }
  ;

range_expression_list
  :                                                                 { () }
  | range_expression_list Sym_brack_l range_expression Sym_brack_r  { () }
  ;

width_constant_expression
  : expression    { () }
  ;

// (*A.8.4 Primaries*)

primary
  : function_subroutine_call                           { Expression_function_subroutine_call }
  | number                                             { Expression_number (snd $1, fst $1) }
  | hierarchical_identifier select                     { Expression_select ($1, $2) }
  | concatenation                                      { Expression_concatenation (fst $1, snd $1) }
  | multiple_concatenation                             { $1 }
  | Sym_paren_l mintypmax_expression Sym_paren_r       { $2 }
  ;

select
  : select_0                                            { Select (rev $1) }
  | select_0 Sym_brack_l part_select_range Sym_brack_r  { Select_part_range (rev $1, $3) }
  ;

select_0
  :                                               { [] }
  |  select_0 Sym_brack_l expression Sym_brack_r  { $3 :: $1 }
  ;

// (*A.8.5 Expression left-side values*)

lvalue
  : hierarchical_identifier select       { Lvalue_select ($1, $2) }
  | Sym_brace_l lvalue_list Sym_brace_r  { Lvalue_concatenation $2 }
  ;

lvalue_list
  : lvalue_list_0  { rev $1 }
  ;

lvalue_list_0
  :                         lvalue  { [$1] }
  | lvalue_list_0 Sym_comma lvalue  { $3 :: $1 }
  ;

// (*A.8.6 Operators*)

// (*A.8.7 Numbers*)

number
  : Lit_number             { $1 }
  | Lit_number_unsigned    { $1 }
  ;

unsigned_number
  : Lit_number_unsigned    { () }
  ;

integral_number  // (*XXX*)
  : number  { $1 }
  ;

// (*A.8.8 Strings*)

// (*A.9 General*)

// (*A.9.1 Attributes*)

//attribute_instance
//  : Sym_paren_l_aster attr_spec_list Sym_aster_paren_r    { () }
//  ;

-}

attribute_instance_list = return () --XXX
--  | attribute_instance_list attribute_instance    { () }

{-

//attr_spec
//  : attr_name expression_default   { () }
//  ;
//
//attr_spec_list
//  :                          attr_spec    { () }
//  | attr_spec_list Sym_comma attr_spec    { () }
//  ;
//
//attr_name
//  : identifier    { () }
//  ;

// (*A.9.2 Comments*)

// (*A.9.3 Identifiers*)

arrayed_identifier
  : identifier range_option  { () }
  ;

hierarchical_identifier
  : hierarchical_branch    { $1 }
  ;

-}

identifier = tokPosStr Id_simple <|> tokPosStr Id_escaped

{-

system_identifier
  : Id_system   { $1 }
  ;
 
// (*A.9.4 Identifier branches*)

hierarchical_branch
  : hierarchical_branch_0    { rev $1 }
  ;

hierarchical_branch_0 
  :                               identifier                                            { [$1] }
//(*XXX*)  |                               identifier Sym_brack_l unsigned_number Sym_brack_r    { () }
  | hierarchical_branch_0 Sym_dot identifier                                            { $3 :: $1 }
//(*XXX*)  | hierarchical_branch_0 Sym_dot identifier Sym_brack_l unsigned_number Sym_brack_r    { () }
  ;

// simple_hierarchical_branch 
//   : simple_identifier [ Sym_brack_l unsigned_number Sym_brack_r ] [ { Sym_dot simple_identifier [ Sym_brack_l unsigned_number Sym_brack_r ] } ]
//   ;
// 
// escaped_hierarchical_branch
//   : escaped_identifier [ Sym_brack_l unsigned_number Sym_brack_r ] [ { Sym_dot escaped_identifier [ Sym_brack_l unsigned_number Sym_brack_r ] } ]
//   ;

// (*A.9.5 White space*)



%%
(* Post Code *)


-}

-}







