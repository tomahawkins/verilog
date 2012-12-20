module Language.Verilog.Simulator
  ( simulate
  , Assignment (..)
  , Path
  , AExpr (..)
  ) where

import Control.Monad
import Control.Monad.State
import Data.List
import System.Exit
import System.IO
import Text.Printf

import Data.VCD ()

import Language.Verilog.AST

-- | Simulation given the top level module name, a list of modules, and a test bench function.
simulate :: [Module] -> Identifier -> Identifier -> IO ()
simulate modules top clock = do
  cs <- execStateT (compileModule topModule) (CompilerState 0 modules [top] top [] [] False clock)
  when (hitError cs) exitFailure
  return ()
  where
  topModule = case lookupModule top modules of
    Nothing -> error $ "Top module not found: " ++ top
    Just m  -> m

lookupModule :: Identifier -> [Module] -> Maybe Module
lookupModule name modules = case [ m | m@(Module n _ _ ) <- modules, name == n ] of
  [m] -> Just m
  []  -> Nothing
  _   -> error $ "Duplicate modules of same name: " ++ name


data CompilerState = CompilerState
  { nextId      :: Int
  , modules     :: [Module]
  , path        :: Path
  , moduleName  :: Identifier
  , _assigns     :: [Assignment]
  , environment :: [(Identifier, Var)]
  , hitError    :: Bool
  , clock       :: Identifier
  }

type VC = StateT CompilerState IO

compileModule :: Module -> VC ()
compileModule (Module _ _ items) = mapM_ compileModuleItem items

extendEnv :: Int -> Identifier -> VC ()
extendEnv width name = do
  c <- get
  put c { nextId = nextId c + 1, environment = (name, Var (nextId c) width [path c ++ [name]]) : environment c }

extendEnv' :: Maybe Range -> [(Identifier, Maybe Range)] -> VC ()
extendEnv' range vars = case range of
  Nothing                       -> mapM_ (f 1)         vars
  Just (Number msb, Number "0") -> mapM_ (f $ num msb) vars
  Just r -> error' $ "Invalid range in variable declaration: " ++ show r
  where
  num :: String -> Int
  num = read  -- XXX This will fail on 'h format.
  f :: Int -> (Identifier, Maybe Range) -> VC ()
  f width (var, array) = case array of
    Nothing -> extendEnv width var
    Just _  -> error' $ "Arrays not supported: " ++ var

compileModuleItem :: ModuleItem -> VC ()
compileModuleItem a = case a of
  Parameter _ _ _ -> return ()  --XXX If parameter is present in environment, check width.  If not, add to environment.
  Inout   _ _ -> error' "inout not supported."
  Input  range vars -> extendEnv' range vars
  Output range vars -> extendEnv' range vars
  Wire   range vars -> extendEnv' range vars
  Reg    range vars -> extendEnv' range vars

  Initial _ -> warning "initial statement ignored."
  Always     sense stmt -> do
    s <- checkSense sense
    case s of
      Combinational -> compileCombStmt stmt
      Posedge       -> compileSeqStmt  stmt
      Negedge       -> warning "negedge always block ignored."

  Assign (LHS _) _ -> return () --XXX
  Assign l       _ -> error' $ "Unsupported assignment LHS: " ++ show l

  Instance mName _parameters iName _bindings -> do
    c <- get 
    case lookupModule mName $ modules c of
      Nothing -> return () --XXX Need to handle externals.
      Just m -> do
        c0 <- get
        put c0 { moduleName = mName, path = path c0 ++ [iName], environment = [] }
        --XXX Insert parameters into environment.
        compileModule m
        --XXX Do bindings after module elaboration.
        c1 <- get
        put c1 { moduleName = moduleName c0, path = path c0, environment = environment c0 }

data SenseType = Combinational | Posedge | Negedge

checkSense :: Sense -> VC SenseType
checkSense sense = case sense of
  Sense   _   -> return Combinational
  SenseOr a b -> do
    a <- checkSense a
    b <- checkSense b
    case (a, b) of
      (Combinational, Combinational) -> return Combinational
      _ -> invalid
  SensePosedge (LHS a) -> do
    c <- get
    if a == clock c then return Posedge else invalid
  SenseNegedge _ -> return Negedge
  _ -> invalid
  where
  invalid = do
    error' $ "Unsupported sense: " ++ show sense
    return Combinational

compileCombStmt :: Stmt -> VC ()
compileCombStmt = compileStmt False Nothing

compileSeqStmt :: Stmt -> VC ()
compileSeqStmt = compileStmt True Nothing

compileStmt :: Bool -> (Maybe Expr) -> Stmt -> VC ()
compileStmt seq guard stmt = case stmt of

  Block _ stmts -> mapM_ (compileStmt seq guard) stmts

  Case scrutee cases def -> compileStmt seq guard $ f cases
    where
    f :: [([Expr], Stmt)] -> Stmt  -- Convert case to an if statement.
    f a = case a of
      [] -> def
      (values, stmt) : rest -> If (foldl1 Or [ Eq value scrutee | value <- values ]) stmt $ f rest

  BlockingAssignment    (LHS _) _ | not seq -> return () --XXX
  NonBlockingAssignment (LHS _) _ | seq     -> return () --XXX

  If pred onTrue onFalse -> do
    compileStmt seq (Just       guard') onTrue
    compileStmt seq (Just $ Not guard') onFalse
    where
    guard' = case guard of
      Nothing -> pred
      Just a  -> And a pred

  Null -> return ()
  _ -> error' $ printf "Unsupported statement in %s always block: %s" (if seq then "sequential" else "combinational") (show stmt)

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

-- | A Path is a hierarchical path of identifiers.
type Path = [Identifier]

-- | A sequence of variable assignments and memory updates in A-normal form.
data Assignment
  = AssignVar Var AExpr
  | AssignReg Var AExpr
  | AssignMem Var AExpr AExpr

data Var = Var Int Int [Path]  -- ^ Uid, width, path list of all signals tied together.

data AExpr
  = AConst      Int Integer  -- ^ Width, value.
  | ASelect     Var Int Int  -- ^ LSB is 0.
  | ABWNot      Var
  | ABWAnd      Var Var
  | ABWXor      Var Var
  | ABWOr       Var Var
  | AMul        Var Var
  | AAdd        Var Var
  | ASub        Var Var
  | AShiftL     Var Int
  | AShiftR     Var Int
  | AEq         Var Var
  | ANe         Var Var
  | ALt         Var Var
  | ALe         Var Var
  | AGt         Var Var
  | AGe         Var Var
  | AMux        Var Var Var
  | AConcat     [Var]

