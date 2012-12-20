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
  { nextVar     :: Int
  , modules     :: [Module]
  , path        :: Path
  , moduleName  :: Identifier
  , assigns     :: [Assignment]
  , environment :: [(Identifier, Var)]
  , hitError    :: Bool
  , clock       :: Identifier
  }

type VC = StateT CompilerState IO

compileModule :: Module -> VC ()
compileModule (Module name _ items) = mapM_ compileModuleItem items

compileModuleItem :: ModuleItem -> VC ()
compileModuleItem a = case a of
  {-
  Paremeter (Maybe Range) Identifier Expr
  Input     (Maybe Range) [(Identifier, Maybe Range)]
  Output    (Maybe Range) [(Identifier, Maybe Range)]
  -}
  Inout     _ _ -> error' "inout not supported."
  {-
  Wire      (Maybe Range) [(Identifier, Maybe Range)]
  Reg       (Maybe Range) [(Identifier, Maybe Range)]
  -}
  Initial    _ -> warning "initial statement ignored."
  Always     sense stmt -> do
    s <- checkSense sense
    case s of
      Combinational -> compileCombStmt stmt
      Posedge       -> compileSeqStmt  stmt
      Negedge       -> warning "negedge always block ignored."

  Assign (LHS v) a -> return () --XXX
  Assign l       _ -> error' $ "Unsupported assignment LHS: " ++ show l

  Instance mName parameters iName bindings -> do
    c <- get 
    case lookupModule mName $ modules c of
      Nothing -> return () --XXX Need to handle externals.
      Just m -> do
        c0 <- get
        put c0 { moduleName = mName, path = path c0 ++ [iName] }
        compileModule m
        c1 <- get
        put c1 { moduleName = moduleName c0, path = path c0 }
  _ -> return ()
  where
  cmi = compileModuleItem

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
compileCombStmt = compileStmt --XXX

compileSeqStmt :: Stmt -> VC ()
compileSeqStmt = compileStmt --XXX

compileStmt :: Stmt -> VC ()
compileStmt stmt = case stmt of
  Block Nothing stmts -> mapM_ compileStmt stmts
  Block (Just b) stmts -> do
    modify $ \ c -> c { path = path c ++ [b] }
    mapM_ compileStmt stmts
    modify $ \ c -> c { path = init $ path c }
  Case                  a b c -> mapM_ compileStmt $ (snd $ unzip b) ++ [c]  --XXX Not correct.
  BlockingAssignment    (LHS _) _ -> return () --XXX
  NonBlockingAssignment (LHS _) _ -> return () --XXX
  If                    a b c -> mapM_ compileStmt [b, c] --XXX Not correct.
  Null -> return ()
  _ -> error' $ "Unsupported statement: " ++ show stmt

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

data Var = Var Int Int (Maybe Int) [Path]  -- ^ Uid, width, array length, path list of all signals tied together.

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

