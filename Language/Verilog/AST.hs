module Language.Verilog.AST
  ( Identifier
  , Module     (..)
  , ModuleItem (..)
  , Stmt       (..)
  , LHS        (..)
  , Expr       (..)
  , Sense      (..)
  , Call       (..)
  , PortBinding
  , Case
  , Range
  ) where

import Data.List
import Data.Maybe
import Text.Printf

import Data.BitVec

type Identifier = String

data Module = Module Identifier [Identifier] [ModuleItem] deriving Eq

instance Show Module where
  show (Module name ports items) = unlines
    [ "module " ++ name ++ (if null ports then "" else "(" ++ commas ports ++ ")") ++ ";"
    , unlines' $ map show items
    , "endmodule"
    ]

data ModuleItem
  = Parameter (Maybe Range) Identifier Expr
  | Input     (Maybe Range) [(Identifier, Maybe Range)]
  | Output    (Maybe Range) [(Identifier, Maybe Range)]
  | Inout     (Maybe Range) [(Identifier, Maybe Range)]
  | Wire      (Maybe Range) [(Identifier, Maybe Range)]
  | Reg       (Maybe Range) [(Identifier, Maybe Range)]
  | Initial    Stmt
  | Always     Sense Stmt
  | Assign     LHS Expr
  | Instance   Identifier [PortBinding] Identifier [PortBinding]
  deriving Eq

type PortBinding = (Identifier, Maybe Expr)

instance Show ModuleItem where
  show a = case a of
    Parameter r n e -> printf "parameter %s%s = %s;" (showRange r) n (show e)
    Input     r a   -> printf "input  %s%s;" (showRange r) (commas [ a ++ showRange r | (a, r) <- a ])
    Output    r a   -> printf "output %s%s;" (showRange r) (commas [ a ++ showRange r | (a, r) <- a ])
    Inout     r a   -> printf "inout  %s%s;" (showRange r) (commas [ a ++ showRange r | (a, r) <- a ])
    Wire      r a   -> printf "wire   %s%s;" (showRange r) (commas [ a ++ showRange r | (a, r) <- a ])
    Reg       r a   -> printf "reg    %s%s;" (showRange r) (commas [ a ++ showRange r | (a, r) <- a ])
    Initial   a     -> printf "initial\n%s" $ indent $ show a
    Always    a b   -> printf "always @(%s)\n%s" (show a) $ indent $ show b
    Assign    a b   -> printf "assign %s = %s;" (show a) (show b)
    Instance  m params i ports
      | null params -> printf "%s %s %s;"     m                    i (showPorts ports)
      | otherwise   -> printf "%s #%s %s %s;" m (showPorts params) i (showPorts ports)
    where
    showPorts :: Show a => [(Identifier, Maybe a)] -> String
    showPorts ports = printf "(%s)" $ commas [ printf ".%s(%s)" i (if isJust arg then show $ fromJust arg else "") | (i, arg) <- ports ]
  
showRange :: Maybe Range -> String
showRange Nothing = ""
showRange (Just (h, l)) = printf "[%s:%s] " (show h) (show l)

indent :: String -> String
indent a = '\t' : f a
  where
  f [] = []
  f (a : rest)
    | a == '\n' = "\n\t" ++ f rest
    | otherwise = a : f rest

unlines' :: [String] -> String
unlines' = intercalate "\n"

data Expr
  = String     String
  | Number     BitVec
  | ConstBool  Bool
  | ExprLHS    LHS
  | ExprCall   Call
  | Not        Expr
  | And        Expr Expr
  | Or         Expr Expr
  | BWNot      Expr
  | BWAnd      Expr Expr
  | BWXor      Expr Expr
  | BWOr       Expr Expr
  | Mul        Expr Expr
  | Div        Expr Expr
  | Mod        Expr Expr
  | Add        Expr Expr
  | Sub        Expr Expr
  | ShiftL     Expr Expr
  | ShiftR     Expr Expr
  | Eq         Expr Expr
  | Ne         Expr Expr
  | Lt         Expr Expr
  | Le         Expr Expr
  | Gt         Expr Expr
  | Ge         Expr Expr
  | Mux        Expr Expr Expr
  | Repeat     Expr [Expr]
  | Concat     [Expr]
  deriving Eq

instance Show Expr where
  show a = case a of
    String     a -> printf "\"%s\"" a
    Number     a -> printf "%d'd%d" (width a) (value a)
    ConstBool  a -> printf "1'b%s" (if a then "1" else "0")
    ExprLHS    a -> show a
    ExprCall   a -> show a
    Not        a -> printf "(! %s)" $ show a
    And        a b -> printf "(%s && %s)" (show a) (show b)
    Or         a b -> printf "(%s || %s)" (show a) (show b)
    BWNot      a -> printf "(~ %s)" $ show a
    BWAnd      a b -> printf "(%s & %s)"  (show a) (show b)
    BWXor      a b -> printf "(%s ^ %s)"  (show a) (show b)
    BWOr       a b -> printf "(%s | %s)"  (show a) (show b)
    Mul        a b -> printf "(%s * %s)"  (show a) (show b)
    Div        a b -> printf "(%s / %s)"  (show a) (show b)
    Mod        a b -> printf "(%s % %s)"  (show a) (show b)
    Add        a b -> printf "(%s + %s)"  (show a) (show b)
    Sub        a b -> printf "(%s - %s)"  (show a) (show b)
    ShiftL     a b -> printf "(%s << %s)" (show a) (show b)
    ShiftR     a b -> printf "(%s >> %s)" (show a) (show b)
    Eq         a b -> printf "(%s == %s)" (show a) (show b)
    Ne         a b -> printf "(%s != %s)" (show a) (show b)
    Lt         a b -> printf "(%s < %s)"  (show a) (show b)
    Le         a b -> printf "(%s <= %s)" (show a) (show b)
    Gt         a b -> printf "(%s > %s)"  (show a) (show b)
    Ge         a b -> printf "(%s >= %s)" (show a) (show b)
    Mux        a b c -> printf "(%s ? %s : %s)" (show a) (show b) (show c)
    Repeat     a b   -> printf "{%s {%s}}" (show a) (commas $ map show b)
    Concat     a     -> printf "{%s}" (commas $ map show a)

data LHS
  = LHS      Identifier
  | LHSBit   Identifier Expr
  | LHSRange Identifier Range
  deriving Eq

instance Show LHS where
  show a = case a of
    LHS        a        -> a
    LHSBit     a b      -> printf "%s[%s]"    a (show b)
    LHSRange   a (b, c) -> printf "%s[%s:%s]" a (show b) (show c)

data Stmt
  = Block                 (Maybe Identifier) [Stmt]
  | Integer               Identifier
  | Case                  Expr [Case] Stmt
  | BlockingAssignment    LHS Expr
  | NonBlockingAssignment LHS Expr
  | For                   (Identifier, Expr) Expr (Identifier, Expr) Stmt
  | If                    Expr Stmt Stmt
  | StmtCall              Call
  | Delay                 BitVec Stmt
  | Null
  deriving Eq

commas :: [String] -> String
commas = intercalate ", "

instance Show Stmt where
  show a = case a of
    Block                 Nothing  b        -> printf "begin\n%s\nend" $ indent $ unlines' $ map show b
    Block                 (Just a) b        -> printf "begin : %s\n%s\nend" a $ indent $ unlines' $ map show b
    Integer               a                 -> printf "integer %s;" a
    Case                  a b c             -> printf "case (%s)\n%s\n\tdefault:\n%s\nendcase" (show a) (indent $ unlines' $ map showCase b) (indent $ indent $ show c)
    BlockingAssignment    a b               -> printf "%s = %s;" (show a) (show b)
    NonBlockingAssignment a b               -> printf "%s <= %s;" (show a) (show b)
    For                   (a, b) c (d, e) f -> printf "for (%s = %s; %s; %s = %s)\n%s" a (show b) (show c) d (show e) $ indent $ show f
    If                    a b Null          -> printf "if (%s)\n%s"           (show a) (indent $ show b)
    If                    a b c             -> printf "if (%s)\n%s\nelse\n%s" (show a) (indent $ show b) (indent $ show c)
    StmtCall              a                 -> printf "%s;" (show a)
    Delay                 a b               -> printf "#%d %s" (value a) (show b)
    Null                                    -> ";"

type Case = ([Expr], Stmt)

showCase :: Case -> String
showCase (a, b) = printf "%s:\n%s" (commas $ map show a) (indent $ show b)

data Call = Call Identifier [Expr] deriving Eq

instance Show Call where
  show (Call a b) = printf "%s(%s)" a (commas $ map show b)

data Sense
  = Sense        LHS
  | SenseOr      Sense Sense
  | SensePosedge LHS
  | SenseNegedge LHS
  deriving Eq

instance Show Sense where
  show a = case a of
    Sense        a -> show a
    SenseOr      a b -> printf "%s or %s" (show a) (show b)
    SensePosedge a   -> printf "posedge %s" (show a)
    SenseNegedge a   -> printf "negedge %s" (show a)

type Range = (Expr, Expr)

