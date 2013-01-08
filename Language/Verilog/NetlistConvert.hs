module Language.Verilog.NetlistConvert
  ( netlist
  ) where

import Control.Monad
import Control.Monad.State
import Data.List
import System.Exit
import System.IO
import Text.Printf

import Language.Verilog.AST
import Language.Verilog.Netlist hiding (Reg)
import qualified Language.Verilog.Netlist as N

data CompilerState = CompilerState
  { nextId      :: NetId
  , modules     :: [Module]
  , path        :: Path
  , moduleName  :: Identifier
  , assignments :: Netlist
  , environment :: [(Identifier, (NetId, Width))]
  , hitError    :: Bool
  }

type VC = StateT CompilerState IO

-- | Netlist a collection of modules given a top level module name.
netlist :: [Module] -> Identifier -> IO Netlist
netlist modules top = do
  cs <- execStateT (compileModule topModule) $ initialCompilerState modules top topModule
  when (hitError cs) exitFailure
  return $ assignments cs
  where
  topModule = case lookupModule top modules of
    Nothing -> error $ "Top module not found: " ++ top
    Just m  -> m

initialCompilerState :: [Module] -> Identifier -> Module -> CompilerState
initialCompilerState modules top topModule  = CompilerState
  { nextId      = length inputs * 2
  , modules     = modules
  , path        = [top]
  , moduleName  = top
  , assignments = concat [ [Var i w [] AInput, N.Reg j w [[top, n]] i]
                         | (i, j, (n, w)) <- zip3 [0, 2 ..] [1, 3 ..] inputs
                         ]
  , environment = [ (n, (i, w)) | (i, (n, w)) <- zip [1, 3 ..] inputs ]
  , hitError    = False
  }
  where
  inputs = moduleInputs topModule

lookupModule :: Identifier -> [Module] -> Maybe Module
lookupModule name modules = case [ m | m@(Module n _ _ ) <- modules, name == n ] of
  [m] -> Just m
  []  -> Nothing
  _   -> error $ "Duplicate modules of same name: " ++ name

compileModule :: Module -> VC ()
compileModule (Module _ _ items) = mapM_ compileModuleItem items

extendEnv :: Int -> Identifier -> VC ()
extendEnv width name = do
  modify $ \ c -> if elem name $ fst $ unzip $ environment c then c else c { nextId = nextId c + 1, environment = (name, (nextId c, width)) : environment c }

extendEnv' :: Maybe Range -> [(Identifier, Maybe Range)] -> VC ()
extendEnv' range vars = mapM_ (f $ width range) vars
  where
  f :: Int -> (Identifier, Maybe Range) -> VC ()
  f width (var, array) = case array of
    Nothing -> extendEnv width var
    Just _  -> error' $ "Arrays not supported: " ++ var

getNetId :: Identifier -> VC (NetId, Width)
getNetId a = do
  c <- get
  getNetId' c a

getNetId' :: CompilerState -> Identifier -> VC (NetId, Width)
getNetId' c a = do
  case lookup a $ environment c of
    Nothing -> do
      error' $ "Variable not found: " ++ a
      return (-1, 0)
    Just a -> return a

-- | Assign an expression to a variable.
assignVar :: (Identifier, Expr) -> VC ()
assignVar (v, e) = do
  (i, w) <- getNetId v
  e <- compileExpr e
  modify $ \ c -> c { assignments = assignments c ++ [Var i w [path c ++ [v]] e] }

assignReg :: (Identifier, Expr) -> VC ()
assignReg (q, e) = do
  (i, w) <- getNetId q
  e   <- compileExpr e
  modify $ \ c -> c { nextId = nextId c + 1, assignments = assignments c ++ [Var (nextId c) w [] e, N.Reg i w [path c ++ [q]] (nextId c)] }

compileModuleItem :: ModuleItem -> VC ()
compileModuleItem a = case a of
  Parameter _ _ _ -> return () -- Parameters bound at instantiation.
  Inout  _ _ -> error' "inout not supported."
  Input  _ _ -> return ()  -- Inputs added to environment at instantiation.
  Output range vars -> extendEnv' range vars
  Wire   range vars -> extendEnv' range vars
  Reg    range vars -> extendEnv' range vars

  Initial _ -> warning "initial statement ignored."
  Always     sense stmt -> do
    s <- checkSense sense
    case s of
      Combinational -> compileCombStmt stmt
      Posedge clk   -> compileSeqStmt clk stmt
      Negedge _     -> warning "negedge always block ignored."

  Assign (LHS v) e -> assignVar (v, e)
  Assign _ _ -> error' $ "Invalid assignment: " ++ show a

  Instance mName parameters iName bindings' -> do
    c <- get 
    case lookupModule mName $ modules c of
      Nothing -> return () --XXX Need to handle externals.
      Just m -> do
        c0 <- get

        -- Bind inputs.
        let env = [ (v, (i, w)) | (i, (v, w)) <- zip [nextId c ..] (moduleInputs m) ]
        modify $ \ c -> c { nextId = nextId c + length env }
        mapM_ (bindSubInput iName bindings) env

        -- Compile sub module.
        modify $ \ c -> c { path = path c ++ [iName], environment = env }
        compileModule m
        c1 <- get
        modify $ \ c -> c { path = path c0, environment = environment c0 }

        -- Bind outputs.
        let outputs' = [ (v, (i, w)) | (v, (i, w)) <- environment c1, elem v outputs ]
        mapM_ (bindSubOutput iName bindings) outputs'

        where
        Module _ _ items = m
        outputs :: [Identifier]
        outputs = concat [ fst $ unzip vars | Output _ vars <- items ]
    where
    bindings = parameters ++ bindings'

bindSubInput :: Identifier -> [(Identifier, Maybe Expr)] -> (Identifier, (NetId, Width)) -> VC () 
bindSubInput iName bindings (v, (i, w)) = case lookup v bindings of
  Nothing       -> error' $ "Unbound input or parameter: " ++ show v
  Just Nothing  -> error' $ "Unbound input or parameter: " ++ show v
  Just (Just e) -> do
    e <- compileExpr e
    modify $ \ c -> c { assignments = assignments c ++ [Var i w [path c ++ [iName, v]] e] }

bindSubOutput :: Identifier -> [(Identifier, Maybe Expr)] -> (Identifier, (NetId, Width)) -> VC ()
bindSubOutput iName bindings (v, (i, _)) = case lookup v bindings of
  Nothing      -> return ()
  Just Nothing -> return ()
  Just (Just (ExprLHS (LHS v))) -> do
    (j, w) <- getNetId v
    modify $ \ c -> c { assignments = assignments c ++ [Var j w [path c ++ [v]] $ AVar i] }
  _ -> error' $ "Invalid output port binding expression in instance " ++ show iName ++ ": " ++ show v

moduleInputs :: Module -> [(Identifier, Int)]
moduleInputs (Module _ _ items) = concat [ [ (var, width range) | (var, _) <- vars ] | Input range vars <- items ] ++
  [ (var, width range) | Parameter range var _ <- items ]

width :: Maybe Range -> Int
width a = case a of
  Nothing -> 1
  Just (Number msb, Number "0") -> read msb + 1
  Just r -> error $ "Invalid range in variable declaration: " ++ show r

data SenseType = Combinational | Posedge Identifier | Negedge Identifier

checkSense :: Sense -> VC SenseType
checkSense sense = case sense of
  Sense   _   -> return Combinational
  SenseOr a b -> do
    a <- checkSense a
    b <- checkSense b
    case (a, b) of
      (Combinational, Combinational) -> return Combinational
      _ -> invalid
  SensePosedge (LHS a) -> return $ Posedge a
  SenseNegedge (LHS a) -> return $ Negedge a
  _ -> invalid
  where
  invalid = do
    error' $ "Unsupported sense: " ++ show sense
    return Combinational

compileCombStmt :: Stmt -> VC ()
compileCombStmt a = compileStmt False a >>= mapM_ assignVar

compileSeqStmt :: Identifier -> Stmt -> VC ()
compileSeqStmt _clk a = compileStmt True a >>= mapM_ assignReg

compileStmt :: Bool -> Stmt -> VC [(Identifier, Expr)]
compileStmt seq stmt = case stmt of

  Block _ stmts -> mapM (compileStmt seq) stmts >>= return . concat

  Case scrutee cases def -> compileStmt seq $ f cases
    where
    f :: [([Expr], Stmt)] -> Stmt
    f a = case a of
      [] -> def
      (values, stmt) : rest -> If (foldl1 Or [ Eq value scrutee | value <- values ]) stmt $ f rest

  BlockingAssignment    (LHS v) e | not seq -> return [(v, e)]
  NonBlockingAssignment (LHS v) e | seq     -> return [(v, e)]

  If pred onTrue onFalse -> do
    t <- compileStmt seq onTrue
    f <- compileStmt seq onFalse
    mapM (merge t f) $ nub $ fst $ unzip $ t ++ f
    where
    merge :: [(Identifier, Expr)] -> [(Identifier, Expr)] -> Identifier -> VC (Identifier, Expr)
    merge t f v = case (lookup v t, lookup v f) of
      (Nothing, Nothing) -> error "Simulator: Should be not get here."
      (Just a , Just b ) -> return (v, Mux pred a b)
      (Just a , Nothing)
        | seq       -> return (v, Mux pred a                 (ExprLHS $ LHS v))
        | otherwise -> do
            warning $ printf "Branch in combinational always block is missing assignment for variable %s.  Assigning to zero." v
            return (v, Mux pred a $ Number "0")
      (Nothing, Just b )
        | seq -> return (v, Mux pred (ExprLHS $ LHS v) b                )
        | otherwise -> do
            warning $ printf "Branch in combinational always block is missing assignment for variable %s.  Assigning to zero." v
            return (v, Mux pred b $ Number "0")

  Null -> return []
  _ -> do
    error' $ printf "Unsupported statement in %s always block: %s" (if seq then "sequential" else "combinational") (show stmt)
    return []

compileExpr :: Expr -> VC AExpr
compileExpr expr = case expr of
  {-
  String     String
  Number     String
  ConstBool  Bool
  ExprLHS    LHS
  Var
  VarBit
  VarRange
  ExprCall   Call
  Not        Expr
  And        Expr Expr
  Or         Expr Expr
  BWNot      Expr
  BWAnd      Expr Expr
  BWXor      Expr Expr
  BWOr       Expr Expr
  Mul        Expr Expr
  Div        Expr Expr
  Mod        Expr Expr
  Add        Expr Expr
  Sub        Expr Expr
  ShiftL     Expr Expr
  ShiftR     Expr Expr
  Eq         Expr Expr
  Ne         Expr Expr
  Lt         Expr Expr
  Le         Expr Expr
  Gt         Expr Expr
  Ge         Expr Expr
  Mux        Expr Expr Expr
  Repeat     Expr [Expr]
  Concat     [Expr]
  -}
  _ -> do
    --error' $ "Unsupported expression: " ++ show expr
    return $ AConst 0 0

warning :: String -> VC ()
warning msg = do
  c <- get
  liftIO $ do
    printf "Warning (module: %s) (instance: %s) : %s\n" (moduleName c) (intercalate "." $ path c) msg
    hFlush stdout

error' :: String -> VC ()
error' msg = do
  c <- get
  liftIO $ do
    printf "ERROR   (module: %s) (instance: %s) : %s\n" (moduleName c) (intercalate "." $ path c) msg
    hFlush stdout
  put c { hitError = True }

