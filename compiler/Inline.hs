module Inline (inline) where

import Data.List

import Language.Verilog

inline :: [Module] -> Module
inline libs = Module topName [] $ moduleItems (initDB topName libs) topItems
  where
  Module topName _ topItems = case [ m | m@(Module _ [] _) <- libs ] of
    [] -> error "Top module not found.  All modules provided have ports."
    a : _  -> a

data DB = DB
  { libs          :: [Module]
  , params        :: [(Identifier, Expr)]
  , ports         :: [(Identifier, Expr)]
  , isTop         :: Bool
  , path          :: [Identifier]
  }

initDB :: Identifier -> [Module] -> DB
initDB top libs = DB
  { libs   = libs
  , isTop  = True
  , params = []
  , ports  = []
  , path   = [top]
  }

moduleItems :: DB -> [ModuleItem] -> [ModuleItem]
moduleItems db items = concatMap moduleItem items
  where
  regNames = [ n | Reg _ [(n, _)] <- items ]
  moduleItem :: ModuleItem -> [ModuleItem]
  moduleItem a = do
    case a of
      Comment a -> [Comment a]
  
      -- Convert parameters to localparams.
      Parameter  a b c -> if (isTop db) 
        then error "Parameters not allowed at top level."
        else [Localparam (maybeRange a) (identifier b) (parameter b $ expr c)]
  
      Localparam a b c -> [Localparam (maybeRange a) (identifier b) (expr c)]
  
      Input a b -> if isTop db
        then error "Ports not allowed at top level."
        else [Wire (maybeRange a) $ flip map b $ \ b -> case lookup b $ ports db of
          Nothing -> error "Unbound input."
          Just c  -> (identifier b, Just c)
          ]
  
      Output a b -> if isTop db
        then error "Ports not allowed at top level."
        else flip concatMap b $ \ b -> case lookup b $ ports db of
            Just (Ident c)
              | elem b regNames -> [                                               Assign (LHS c) $ Ident $ identifier b]  --XXX Is it okay if the reg declaration is after the assignment?
              | otherwise       -> [Wire (maybeRange a) [(identifier b, Nothing)], Assign (LHS c) $ Ident $ identifier b]
            Just (IdentBit c d)
              | elem b regNames -> [                                               Assign (LHSBit c d) $ Ident $ identifier b]  --XXX Is it okay if the reg declaration is after the assignment?
              | otherwise       -> [Wire (maybeRange a) [(identifier b, Nothing)], Assign (LHSBit c d) $ Ident $ identifier b]
            Just (IdentRange c d)
              | elem b regNames -> [                                               Assign (LHSRange c d) $ Ident $ identifier b]  --XXX Is it okay if the reg declaration is after the assignment?
              | otherwise       -> [Wire (maybeRange a) [(identifier b, Nothing)], Assign (LHSRange c d) $ Ident $ identifier b]
            Nothing -> [Wire (maybeRange a) [(identifier b, Nothing)]]
            Just _  -> error "Invalid output binding."
  
      Inout _ _ -> error "Inout declarations not supported."
      Wire a b -> [Wire (maybeRange a) $ flip map b $ \ (b, c) -> (identifier b, maybeExpr c)]
      Reg a b -> [Reg (maybeRange a) $ flip map b $ \ (b, c) -> (identifier b, maybeRange c)]
      Integer a -> [Integer $ map identifier a]
  
      Initial a -> [Initial $ stmt a]
      Always  Nothing  b -> [Always Nothing $ stmt b]
      Always  (Just a) b -> [Always (Just $ sense a) $ stmt b]
      Assign  a b        -> [Assign (lhs a) (expr b)]
  
      Instance modName params' instName ports' ->
        [Comment $ "( Inlining instance: " ++ show (Instance modName params' instName ports')] ++
        items' ++
        [Comment ")"]
        where
        Module _ _ instItems = case [ m | m@(Module name _ _) <- libs db, name == modName ] of
          [] -> error $ "Module not found: " ++ modName
          _ : _ : _ -> error $ "Multiple modules found with the same name: " ++ modName
          [m] -> m
        items' = moduleItems db
          { isTop = False
          , params = [ (a, expr b) | (a, Just b) <- params' ]
          , ports  = [ (a, expr b) | (a, Just b) <- ports'  ]
          , path   = path db ++ [instName]
          } instItems

  identifier :: Identifier -> Identifier
  identifier a = "\\" ++ intercalate "." (path db ++ [a]) ++ " "

  maybeExpr :: Maybe Expr -> Maybe Expr
  maybeExpr a = a >>= return . expr
  
  maybeRange :: Maybe Range -> Maybe Range
  maybeRange a = a >>= return . range
  
  range :: Range -> Range
  range (a, b) = (expr a, expr b)
  
  lhs :: LHS -> LHS
  lhs a = case a of
    LHS a -> LHS $ identifier a
    LHSBit a b -> LHSBit (identifier a) (expr b)
    LHSRange a b -> LHSRange (identifier a) (range b)
    LHSConcat a  -> LHSConcat $ map lhs a
  
  expr :: Expr -> Expr
  expr a = case a of
    String a -> String a
    Number a -> Number a
    ConstBool a -> ConstBool a
    Ident a -> Ident $ identifier a
    IdentBit a b -> IdentBit (identifier a) (expr b)
    IdentRange a (b, c) -> IdentRange (identifier a) (expr b, expr c)
    Repeat a b -> Repeat (expr a) $ map expr b
    Concat a -> Concat $ map expr a
    ExprCall a -> ExprCall $ call a
    UniOp a b -> UniOp a $ expr b
    BinOp a b c -> BinOp a (expr b) (expr c)
    Mux a b c -> Mux (expr a) (expr b) (expr c)
    Bit a b -> Bit (expr a) b
  
  -- Lookup a parameter of an instance.
  parameter :: Identifier -> Expr -> Expr
  parameter a b = case lookup a $ params db of
    Nothing -> b
    Just a  -> a
  
  sense :: Sense -> Sense
  sense a = case a of
    Sense a -> Sense $ lhs a
    SenseOr a b -> SenseOr (sense a) (sense b)
    SensePosedge a -> SensePosedge $ lhs a
    SenseNegedge a -> SenseNegedge $ lhs a
  
  maybeStmt :: Maybe Stmt -> Maybe Stmt
  maybeStmt a = a >>= return . stmt

  stmt :: Stmt -> Stmt
  stmt a = case a of
    Block a b -> Block a $ map stmt b
    StmtInteger a -> StmtInteger $ map identifier a
    StmtReg a b -> StmtReg (maybeRange a) [ (identifier a, maybeRange b) | (a, b) <- b ]
    Case a b c -> Case (expr a) [ (map expr a, stmt b) | (a, b) <- b ] (maybeStmt c)
    BlockingAssignment    a b -> BlockingAssignment    (lhs a) (expr b)
    NonBlockingAssignment a b -> NonBlockingAssignment (lhs a) (expr b)
    If a b c -> If (expr a) (stmt b) (stmt c)
    StmtCall a -> StmtCall $ call a
    Delay a b -> Delay (expr a) (stmt b)
    Null -> Null
    For (a, b) c (d, e) f -> For (identifier a, expr b) (expr c) (identifier d, expr e) (stmt f)
  
  call :: Call -> Call
  call (Call a b) = Call a $ map expr b

