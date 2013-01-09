module Language.Verilog.NetlistConvert
  ( netlist
  , BlackBoxInterface
  ) where

import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.List
import System.Exit
import System.IO
import Text.Printf

import Data.BitVec hiding (width)
import qualified Data.BitVec as BV
import Language.Verilog.AST
import Language.Verilog.Netlist hiding (Reg)
import qualified Language.Verilog.Netlist as N

data CompilerState = CompilerState
  { nextId      :: NetId
  , modules     :: [Module]
  , blackBoxes  :: [(Path, BlackBoxInterface)]
  , path        :: Path
  , moduleName  :: Identifier
  , nets        :: Netlist
  , environment :: [(Identifier, (NetId, Width))]
  , hitError    :: Bool
  }

type VC = StateT CompilerState IO

checkN :: Int -> String -> VC ()
checkN n msg = return () {-do
  c <- get
  liftIO $ when (nextId c == n) $ do
    putStrLn $ "(instance: " ++ intercalate "." (path c) ++ "): " ++ msg
    hFlush stdout
    -}

-- | Lists of inputs and outputs.
type BlackBoxInterface = ([(Identifier, Width)], [(Identifier, Width)])

-- | Netlist a collection of modules given a top level module name and a mapping of external instance sub-outputs.
netlist :: [Module] -> Identifier -> [(Path, BlackBoxInterface)] -> IO Netlist
netlist modules top blackBoxes = do
  cs <- execStateT (compileModule topModule) $ initialCompilerState modules top blackBoxes topModule
  when (hitError cs) exitFailure
  return $ nets cs
  where
  topModule = case lookupModule top modules of
    Nothing -> error $ "Top module not found: " ++ top
    Just m  -> m

initialCompilerState :: [Module] -> Identifier -> [(Path, BlackBoxInterface)] -> Module -> CompilerState
initialCompilerState modules top blackBoxes topModule  = CompilerState
  { nextId      = length inputs * 2
  , modules     = modules
  , blackBoxes  = blackBoxes
  , path        = [top]
  , moduleName  = top
  , nets        = concat [ [Var i w [] AInput, N.Reg j w [[top, n]] i]
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

extendEnv :: Maybe Range -> [(Identifier, Maybe Range)] -> VC ()
extendEnv range vars = mapM_ (f $ width range) vars
  where
  f :: Int -> (Identifier, Maybe Range) -> VC ()
  f width (var, array) = case array of
    Nothing -> checkN 89 ("extendEnv': " ++ var) >> extendEnv width var
    Just _  -> error' $ "Arrays not supported: " ++ var

  extendEnv :: Int -> Identifier -> VC ()
  extendEnv width name = do
    modify $ \ c -> if elem name $ fst $ unzip $ environment c then c else c { nextId = nextId c + 1, environment = (name, (nextId c, width)) : environment c }

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
  (e, _) <- compileExpr e
  modify $ \ c -> c { nets = Var i w [path c ++ [v]] (AVar e) : nets c }

assignReg :: (Identifier, Expr) -> VC ()
assignReg (q, e) = do
  (i, w) <- getNetId q
  (e, _) <- compileExpr e
  modify $ \ c -> c { nextId = nextId c + 1, nets = [Var (nextId c) w [] $ AVar e, N.Reg i w [path c ++ [q]] (nextId c)] ++ nets c }

compileModuleItem :: ModuleItem -> VC ()
compileModuleItem a = case a of
  Parameter _ _ _ -> return () -- Parameters bound at instantiation.
  Inout  _ _ -> error' "inout not supported."
  Input  _ _ -> return ()  -- Inputs added to environment at instantiation.
  Output range vars -> extendEnv range vars
  Wire   range vars -> extendEnv range vars
  Reg    range vars -> extendEnv range vars

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
      Nothing -> case lookup (path c ++ [iName]) $ blackBoxes c of
        Nothing -> error' $ "Unknown blackbox:  module: " ++ mName ++ "  instance: " ++ intercalate "." (path c ++ [iName])
        Just (inputs, outputs) -> return () --XXX Need to handle externals.

      Just m -> do
        c0 <- get

        -- Bind inputs.
        let env = [ (v, (i, w)) | (i, (v, w)) <- zip [nextId c ..] (moduleInputs m) ]
        modify $ \ c -> c { nextId = nextId c + length env }
        mapM_ (bindSubInput iName bindings) env

        -- Compile sub module.
        modify $ \ c -> c { moduleName = mName, path = path c ++ [iName], environment = env }
        compileModule m
        c1 <- get
        modify $ \ c -> c { moduleName = moduleName c0, path = path c0, environment = environment c0 }

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
    (e, _) <- compileExpr e
    modify $ \ c -> c { nets = Var i w [path c ++ [iName, v]] (AVar e) : nets c }

bindSubOutput :: Identifier -> [(Identifier, Maybe Expr)] -> (Identifier, (NetId, Width)) -> VC ()
bindSubOutput iName bindings (v, (i, _)) = case lookup v bindings of
  Nothing      -> return ()
  Just Nothing -> return ()
  Just (Just (ExprLHS (LHS v))) -> do
    (j, w) <- getNetId v
    modify $ \ c -> c { nets = Var j w [path c ++ [v]] (AVar i) : nets c }
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

compileExpr :: Expr -> VC (NetId, Width)
compileExpr expr = case expr of
  {-
  String     String
  -}
  Number a -> net (BV.width n) (AConst n) where n = number a
  ConstBool  a -> net 1 $ AConst $ if a then 1 else 0
  ExprLHS (LHS v) -> do
    (v, w) <- getNetId v
    net w $ AVar v
  ExprLHS (LHSBit v a) -> do
    (v, _) <- getNetId v
    a <- anf' a
    net 1 $ ASelect v a a
  ExprLHS (LHSRange v (a, b)) -> do
    (v, w) <- getNetId v
    a <- anf' a
    b <- anf' b
    net w $ ASelect v a b
  {-
  ExprCall   Call
  -}
  Not   a   -> anf $ Eq a $ Number "0"
  And   a b -> anf $ BWAnd (Ne a $ Number "0") (Ne b $ Number "0")
  Or    a b -> anf $ BWOr  (Ne a $ Number "0") (Ne b $ Number "0")
  BWNot a   -> do { (a, wa) <- anf a;                   net      wa     $ ABWNot a   }
  BWAnd a b -> do { (a, wa) <- anf a; (b, wb) <- anf b; net (max wa wb) $ ABWAnd a b }
  BWXor a b -> do { (a, wa) <- anf a; (b, wb) <- anf b; net (max wa wb) $ ABWXor a b }
  BWOr  a b -> do { (a, wa) <- anf a; (b, wb) <- anf b; net (max wa wb) $ ABWOr  a b }
  Mul   a b -> do { (a, wa) <- anf a; (b, wb) <- anf b; net (max wa wb) $ AMul   a b }
  {-
  Div        Expr Expr
  Mod        Expr Expr
  -}
  Add    a b -> do { (a, wa) <- anf a; (b, wb) <- anf b; net (max wa wb) $ AAdd a b }
  Sub    a b -> do { (a, wa) <- anf a; (b, wb) <- anf b; net (max wa wb) $ ASub a b }
  ShiftL a b -> do { (a, wa) <- anf a; b <- anf' b; net wa $ AShiftL a b }
  ShiftR a b -> do { (a, wa) <- anf a; b <- anf' b; net wa $ AShiftR a b }
  Eq     a b -> do { a <- anf' a; b <- anf' b; net 1 $ AEq a b }
  Ne     a b -> do { a <- anf' a; b <- anf' b; net 1 $ ANe a b }
  Lt     a b -> do { a <- anf' a; b <- anf' b; net 1 $ ALt a b }
  Le     a b -> do { a <- anf' a; b <- anf' b; net 1 $ ALe a b }
  Gt     a b -> do { a <- anf' a; b <- anf' b; net 1 $ AGt a b }
  Ge     a b -> do { a <- anf' a; b <- anf' b; net 1 $ AGe a b }
  Mux a b c -> do
    (a, _ ) <- anf a
    (b, wb) <- anf b
    (c, wc) <- anf c
    net (max wb wc) $ AMux a b c 
  Repeat (Number a) b -> anf $ Concat $ replicate (fromIntegral $ value $ number a) b'
    where
    b' = Concat b
  Concat a -> case a of
    [] -> undefined
    [a] -> compileExpr a
    a : b -> do
      (a, wa) <- compileExpr a
      (b, wb) <- compileExpr $ Concat b
      net (wa + wb) $ AConcat a b
  _ -> do
    error' $ "Unsupported expression: " ++ show expr
    return (0, 0)
  where

  anf = compileExpr
  anf' a = anf a >>= return . fst

  net :: Width -> AExpr -> VC (NetId, Width)
  net w a = do
    c <- get
    put c { nextId = nextId c + 1, nets = Var (nextId c) w [] a : nets c }
    return (nextId c, w)

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
    | isPrefixOf "'b" a = foldl (\ n b -> shiftL n 1 .|. (if b == '1' then 1 else 0)) 0 (drop 2 a)
    | otherwise         = error $ "Invalid number format: " ++ a

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

check :: String -> VC ()
check msg = do
  c <- get
  liftIO $ do
    printf "Check (module: %s) (instance: %s) : %s\n" (moduleName c) (intercalate "." $ path c) msg
    hFlush stdout


