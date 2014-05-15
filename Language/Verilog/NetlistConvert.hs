module Language.Verilog.NetlistConvert
  ( BlackBox (..)
  , netlist
  ) where

import Control.Monad
import Control.Monad.State
import Data.List
import System.Exit
import System.IO
import Text.Printf

import Data.BitVec hiding (width)
import qualified Data.BitVec as BV
import Language.Verilog.AST
import Language.Verilog.Netlist hiding (Reg)
import qualified Language.Verilog.Netlist as N

type Parameters = [PortBinding]

-- | Details of blackboxes.
data BlackBox = BlackBox
  { bboxModule         :: Identifier
  , bboxIO             :: Parameters -> ([(Identifier, Width)], [(Identifier, Width)])
  , bboxImplementation :: Parameters -> Path -> BlackBoxInit  -- ^ Signal order determined by bboxIO.
  }

data CompilerState = CompilerState
  { nextId        :: NetId
  , modules       :: [Module]
  , blackBoxDefs  :: [BlackBox]
  , blackBoxImpls :: [([NetId], [NetId], BlackBoxInit)]
  , path          :: Path
  , moduleName    :: Identifier
  , nets          :: Netlist BlackBoxInit
  , environment   :: [(Identifier, (NetId, Width))]
  , hitError      :: Bool
  }

type VC = StateT CompilerState IO

checkN :: Int -> String -> VC ()
checkN n msg = do
  c <- get
  liftIO $ when (nextId c == n) $ do
    putStrLn $ "(instance: " ++ intercalate "." (path c) ++ "): " ++ msg
    hFlush stdout

-- | Netlist a design given a list of modules, the top level module name, and all 'BlackBox' implementations.
--   The top level module must not have any ports.
netlist :: [Module] -> Identifier -> [BlackBox] -> IO (Netlist BlackBoxInit)
netlist modules top blackBoxes = do
  c <- execStateT (checkTopModule topModule >> compileModule topModule) CompilerState
    { nextId        = 0
    , modules       = modules
    , blackBoxDefs  = blackBoxes
    , blackBoxImpls = []
    , path          = [top]
    , moduleName    = top
    , nets          = []
    , environment   = []
    , hitError      = False
    }
  when (hitError c) exitFailure
  return $ nets c
  where
  topModule = case lookupModule top modules of
    Nothing -> error $ "Top module not found: " ++ top
    Just m  -> m

checkTopModule :: Module -> VC ()
checkTopModule m = when (not $ null $ moduleInputs m) $ error' "Top level module has IO."

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
    Nothing -> extendEnv width var
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
    c0 <- get 
    let path' = path c0 ++ [iName]
    case lookupModule mName $ modules c0 of

      -- Blackbox instantiation.
      Nothing -> case [ bbox | bbox <- blackBoxDefs c0, bboxModule bbox == mName ] of
        []        -> error' $ "Unknown blackbox:  module: " ++ mName ++ "  instance: " ++ intercalate "." path'
        _ : _ : _ -> error' $ "Multiple blackboxes with same name:  module: " ++ mName ++ "  instance: " ++ intercalate "." path'
        [bbox] -> do
          modify $ \ c -> c { moduleName = mName, path = path' }
          inputs  <- mapM input  $ fst $ bboxIO bbox parameters
          outputs <- mapM output $ snd $ bboxIO bbox parameters
          let impl = bboxImplementation bbox parameters path'
          modify $ \ c -> c { moduleName = moduleName c0, path = path c0, blackBoxImpls = (inputs, outputs, impl) : blackBoxImpls c }
          where
          input :: (Identifier, Width) -> VC NetId
          input (a, _) = case lookup a bindings of
            Nothing      -> error' ("Unbound input: " ++ a) >> return 0
            Just Nothing -> error' ("Unbound input: " ++ a) >> return 0
            Just (Just e) -> do
              (e, _) <- compileExpr e
              return e

          output :: (Identifier, Width) -> VC NetId
          output (a, w) = do
            c <- get
            let outputSrc = nextId c
            put c { nextId = nextId c + 1, nets = Var outputSrc w [path c ++ [a]] AInput : nets c }
            case lookup a bindings of
              Nothing       -> return ()
              Just Nothing  -> return ()
              Just (Just (ExprLHS (LHS v))) -> do
                (n, _) <- getNetId' c0 v
                modify $ \ c -> c { nets = Var n w [init (path c) ++ [v]] (AVar outputSrc) : nets c }
              Just (Just e) -> error' $ "Invalid output port binding expression in instance " ++ show iName ++ ": " ++ show e
            return $ nextId c

      -- Regular module instanitation.
      Just m -> do
        -- Bind inputs.
        let env = [ (v, (i, w)) | (i, (v, w)) <- zip [nextId c0 ..] (moduleInputs m) ]
        modify $ \ c -> c { nextId = nextId c + length env }
        mapM_ (bindSubInput iName bindings) env

        -- Compile sub module.
        modify $ \ c -> c { moduleName = mName, path = path', environment = env }
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
  Just (Number msb, Number lsb) | value lsb == 0 -> fromIntegral $ value msb + 1
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
            return (v, Mux pred a $ Number 0)
      (Nothing, Just b )
        | seq -> return (v, Mux pred (ExprLHS $ LHS v) b                )
        | otherwise -> do
            warning $ printf "Branch in combinational always block is missing assignment for variable %s.  Assigning to zero." v
            return (v, Mux pred b $ Number 0)

  Null -> return []
  _ -> do
    error' $ printf "Unsupported statement in %s always block: %s" (if seq then "sequential" else "combinational") (show stmt)
    return []

compileExpr :: Expr -> VC (NetId, Width)
compileExpr expr = case expr of
  {-
  String     String
  -}
  Number a -> net (BV.width a) (AConst a)
  ConstBool  a -> net 1 $ AConst $ if a then 1 else 0
  ExprLHS (LHS v) -> do
    (v, w) <- getNetId v
    net w $ AVar v
  ExprLHS (LHSBit v a) -> do
    (v, _) <- getNetId v
    a <- anf' a
    net 1 $ ASelect v a a
  ExprLHS (LHSRange v (a, b)) -> do
    (v, _) <- getNetId v
    a <- anf' a
    b <- anf' b
    net w $ ASelect v a b
  {-
  ExprCall   Call
  -}
  Not   a   -> anf $ Eq a $ Number 0
  And   a b -> anf $ BWAnd (Ne a $ Number 0) (Ne b $ Number 0)
  Or    a b -> anf $ BWOr  (Ne a $ Number 0) (Ne b $ Number 0)
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
  Repeat (Number a) b -> anf $ Concat $ replicate (fromIntegral $ value a) b'
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

{-
note :: String -> VC ()
note msg = do
  c <- get
  liftIO $ do
    printf "Note    (module: %s) (instance: %s) : %s\n" (moduleName c) (intercalate "." $ path c) msg
    hFlush stdout
    -}


