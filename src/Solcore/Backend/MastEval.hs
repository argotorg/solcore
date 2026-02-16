module Solcore.Backend.MastEval
  ( evalCompUnit,
    defaultFuel,
    eliminateDeadCode,
  )
where

{- Partial Evaluator for Mast
   Performs compile-time evaluation where possible:
   - Folds calls to `addWord`, `gtWord`, `eqWord` with literal arguments
   - Propagates known variable values
   - Inlines simple pure functions with literal arguments
-}

import Control.Monad.Reader
import Control.Monad.State
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (Keccak_256)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Solcore.Backend.Mast
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt (Literal (..))

-----------------------------------------------------------------------
-- Data structures
-----------------------------------------------------------------------

-- Variable environment: variable id (name + type) -> known value
-- Uses full MastId to distinguish variables with same name but different types
type VEnv = Map.Map MastId MastExp

-- Function table: function name -> definition
type FunTable = Map.Map Name MastFunDef

-- Fuel for controlling recursion depth during inlining
type Fuel = Int

defaultFuel :: Fuel
defaultFuel = 100

-----------------------------------------------------------------------
-- Evaluation monad
-----------------------------------------------------------------------

data EvalEnv = EvalEnv
  { envFunTable :: FunTable,
    envPureFuns :: Set.Set Name
  }

-- Reader for constant environment, State for fuel budget
type EvalM = ReaderT EvalEnv (State Fuel)

runEvalM :: EvalEnv -> Fuel -> EvalM a -> (a, Fuel)
runEvalM env fuel m = runState (runReaderT m env) fuel

askFunTable :: EvalM FunTable
askFunTable = asks envFunTable

askPureFuns :: EvalM (Set.Set Name)
askPureFuns = asks envPureFuns

getFuel :: EvalM Fuel
getFuel = lift get

-- Consume one unit of fuel, returns True if fuel was available
useFuel :: EvalM Bool
useFuel = do
  f <- getFuel
  if f > 0
    then do
      lift $ put (f - 1)
      pure True
    else pure False

-- Restore one unit of fuel (called after successful inlining)
restoreFuel :: EvalM ()
restoreFuel = lift $ modify (+ 1)

-----------------------------------------------------------------------
-- Main entry point
-----------------------------------------------------------------------

-- Returns the evaluated compilation unit and remaining fuel
evalCompUnit :: Fuel -> MastCompUnit -> (MastCompUnit, Fuel)
evalCompUnit fuel cu = (cu {mastTopDecls = decls'}, remainingFuel)
  where
    funTable = buildFunTable cu
    pureFuns = computePureFuns funTable
    env = EvalEnv {envFunTable = funTable, envPureFuns = pureFuns}
    (decls', remainingFuel) = runEvalM env fuel (mapM evalTopDecl (mastTopDecls cu))

-----------------------------------------------------------------------
-- Build function table from compilation unit
-----------------------------------------------------------------------

buildFunTable :: MastCompUnit -> FunTable
buildFunTable cu = Map.fromList $ concatMap collectFromTopDecl (mastTopDecls cu)
  where
    collectFromTopDecl :: MastTopDecl -> [(Name, MastFunDef)]
    collectFromTopDecl (MastTContr c) = collectFromContract c
    collectFromTopDecl (MastTDataDef _) = []

    collectFromContract :: MastContract -> [(Name, MastFunDef)]
    collectFromContract c = concatMap collectFromDecl (mastContrDecls c)

    collectFromDecl :: MastContractDecl -> [(Name, MastFunDef)]
    collectFromDecl (MastCFunDecl fd) = [(mastFunName fd, fd)]
    collectFromDecl (MastCMutualDecl ds) = concatMap collectFromDecl ds
    collectFromDecl (MastCDataDecl _) = []

-----------------------------------------------------------------------
-- Evaluate top-level declarations
-----------------------------------------------------------------------

evalTopDecl :: MastTopDecl -> EvalM MastTopDecl
evalTopDecl (MastTContr c) = MastTContr <$> evalContract c
evalTopDecl d@(MastTDataDef _) = pure d

evalContract :: MastContract -> EvalM MastContract
evalContract c = do
  decls' <- mapM evalContractDecl (mastContrDecls c)
  pure $ c {mastContrDecls = decls'}

evalContractDecl :: MastContractDecl -> EvalM MastContractDecl
evalContractDecl (MastCFunDecl fd) = MastCFunDecl <$> evalFunDef fd
evalContractDecl (MastCMutualDecl ds) = MastCMutualDecl <$> mapM evalContractDecl ds
evalContractDecl d@(MastCDataDecl _) = pure d

-----------------------------------------------------------------------
-- Evaluate function definitions
-----------------------------------------------------------------------

evalFunDef :: MastFunDef -> EvalM MastFunDef
evalFunDef fd = do
  (_, body') <- evalStmts Map.empty (mastFunBody fd)
  pure $ fd {mastFunBody = body'}

-----------------------------------------------------------------------
-- Evaluate statements (AST transformation)
-----------------------------------------------------------------------

-- Transform statements in place, evaluating expressions where possible.
-- Used for optimizing function bodies that remain in the output.
-- Compare with evalFunBody which extracts a return value for inlining.
-- TODO: consider unifying these two statement handlers

-- Process statements left-to-right, threading environment through
evalStmts :: VEnv -> [MastStmt] -> EvalM (VEnv, [MastStmt])
evalStmts env [] = pure (env, [])
evalStmts env (s : ss) = do
  (env', s') <- evalStmt env s
  (env'', ss') <- evalStmts env' ss
  pure (env'', s' <> ss')

evalStmt :: VEnv -> MastStmt -> EvalM (VEnv, [MastStmt])
evalStmt env stmt = case stmt of
  MastLet i ty mInit -> do
    mInit' <- traverse (evalExp env) mInit
    let env' = case mInit' of
          Just e | isKnownValue e -> Map.insert i e env
          _ -> Map.delete i env -- Shadow/remove any existing binding
          -- Always emit the let: the variable may be referenced by opaque asm blocks
    pure (env', [MastLet i ty mInit'])
  MastAssign i e -> do
    e' <- evalExp env e
    let env' =
          if isKnownValue e'
            then Map.insert i e' env
            else Map.delete i env -- Value no longer known
            -- Always emit the assignment: the variable may be referenced by opaque asm blocks
    pure (env', [MastAssign i e'])
  MastStmtExp e -> do
    e' <- evalExp env e
    if isKnownValue e'
      then pure (env, [])
      else pure (env, [MastStmtExp e'])
  MastReturn e -> do
    e' <- evalExp env e
    pure (env, [MastReturn e'])
  MastMatch e alts -> do
    e' <- evalExp env e
    alts' <- mapM (evalAlt env) alts
    pure (env, [MastMatch e' alts'])
  MastAsm yul ->
    -- Assembly blocks are opaque; we don't know what they modify
    -- Conservative: clear all variable bindings
    pure (Map.empty, [MastAsm yul])

evalAlt :: VEnv -> MastAlt -> EvalM MastAlt
evalAlt env (pat, body) = do
  -- Pattern bindings shadow existing bindings, but we don't track them
  -- (conservative: treat all pattern-bound vars as unknown)
  (_, body') <- evalStmts env body
  pure (pat, body')

-----------------------------------------------------------------------
-- Evaluate expressions
-----------------------------------------------------------------------

evalExp :: VEnv -> MastExp -> EvalM MastExp
evalExp _ expr@(MastLit _) = pure expr
evalExp env expr@(MastVar i) =
  pure $ case Map.lookup i env of
    Just lit -> lit
    Nothing -> expr
evalExp env (MastCall i args) = do
  args' <- mapM (evalExp env) args
  let fname = mastIdName i
  case evalPrimitive fname args' of
    Just result -> pure result
    Nothing -> do
      -- Try inlining if we have fuel
      hasFuel <- useFuel
      if hasFuel
        then do
          result <- tryInline fname args'
          restoreFuel -- Restore fuel: it acts purely as recursion depth limit
          pure $ case result of
            Just r -> r
            Nothing -> MastCall i args'
        else pure $ MastCall i args'
evalExp env (MastCon i es) = do
  es' <- mapM (evalExp env) es
  pure $ MastCon i es'
evalExp env (MastCond e1 e2 e3) = do
  -- Evaluate all branches (conservative approach)
  -- Could potentially simplify if condition is known literal
  e1' <- evalExp env e1
  e2' <- evalExp env e2
  e3' <- evalExp env e3
  pure $ MastCond e1' e2' e3'

-----------------------------------------------------------------------
-- Primitive evaluation
-----------------------------------------------------------------------

evalPrimitive :: Name -> [MastExp] -> Maybe MastExp
evalPrimitive (Name "addWord") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just (MastLit (IntLit (a + b)))
evalPrimitive (Name "subWord") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just (MastLit (IntLit (a - b)))
evalPrimitive (Name "gtWord") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just $ mkBool (a > b)
evalPrimitive (Name "eqWord") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just $ mkBool (a == b)
-- String literal primitives (Solidity/Yul semantics):
-- - treat literals as UTF-8 byte sequences
-- - strlen returns byte length
-- - keccak returns the 256-bit big-endian word of keccak256(bytes)
evalPrimitive (Name "concatLit") [MastLit (StrLit a), MastLit (StrLit b)] =
  Just (MastLit (StrLit (a <> b)))
evalPrimitive (Name "strlenLit") [MastLit (StrLit s)] =
  let bs = TE.encodeUtf8 (T.pack s)
   in Just (MastLit (IntLit (toInteger (BS.length bs))))
evalPrimitive (Name "keccakLit") [MastLit (StrLit s)] =
  let bs = TE.encodeUtf8 (T.pack s)
      digest :: Digest Keccak_256
      digest = hash bs
      digestBytes :: BS.ByteString
      digestBytes = BA.convert digest
   in Just (MastLit (IntLit (bsToIntegerBE digestBytes)))
evalPrimitive _ _ = Nothing

bsToIntegerBE :: BS.ByteString -> Integer
bsToIntegerBE = BS.foldl' step 0
  where
    step :: Integer -> Word8 -> Integer
    step acc w = acc * 256 + fromIntegral w

-- Construct a boolean value as sum((), ())
-- true = inr(()), false = inl(())
mkBool :: Bool -> MastExp
mkBool b = MastCon conId [unitVal]
  where
    conName = if b then Name "inr" else Name "inl"
    conTy = MastTyCon (Name "->") [unitTy, boolTy]
    conId = MastId conName conTy
    unitTy = MastTyCon (Name "unit") []
    boolTy = MastTyCon (Name "sum") [unitTy, unitTy]
    unitVal = MastCon (MastId (Name "()") unitTy) []

-----------------------------------------------------------------------
-- Function inlining
-----------------------------------------------------------------------

-- Try to inline a function call.
-- Works when: (1) all arguments are known values, or
--             (2) function is "constant" (ignores its arguments)
-- Only pure functions (no asm, no impure calls) are eligible for inlining.
tryInline :: Name -> [MastExp] -> EvalM (Maybe MastExp)
tryInline fname args = do
  pureFuns <- askPureFuns
  if fname `Set.notMember` pureFuns
    then pure Nothing
    else do
      ft <- askFunTable
      case Map.lookup fname ft of
        Nothing -> pure Nothing
        Just fd
          | length (mastFunParams fd) /= length args -> pure Nothing
          | otherwise -> do
              let params = mastFunParams fd
                  paramToId p = MastId (mastParamName p) (mastParamType p)
                  env = Map.fromList $ zip (map paramToId params) args
              evalFunBody env (mastFunBody fd)

-- Evaluate a function body and extract the return value
evalFunBody :: VEnv -> [MastStmt] -> EvalM (Maybe MastExp)
evalFunBody _ [] = pure Nothing -- No return statement found
evalFunBody env (stmt : rest) = case stmt of
  MastLet i _ mInit -> do
    mInit' <- traverse (evalExp env) mInit
    let env' = case mInit' of
          Just e | isKnownValue e -> Map.insert i e env
          _ -> Map.delete i env
    evalFunBody env' rest
  MastAssign i e -> do
    e' <- evalExp env e
    let env' =
          if isKnownValue e'
            then Map.insert i e' env
            else Map.delete i env
    evalFunBody env' rest
  MastStmtExp _ -> evalFunBody env rest
  MastReturn e -> do
    e' <- evalExp env e
    pure $ if isKnownValue e' then Just e' else Nothing
  MastMatch scrut alts -> do
    scrut' <- evalExp env scrut
    case matchAlts env scrut' alts of
      Just (env', body) -> evalFunBody env' body
      Nothing -> pure Nothing -- Scrutinee not known, can't select branch
  MastAsm _ -> pure Nothing -- Should not happen: purity analysis excludes asm functions

-- Try to match a known scrutinee against alternatives.
-- Returns the extended environment and the body of the matching alternative.
matchAlts :: VEnv -> MastExp -> [MastAlt] -> Maybe (VEnv, [MastStmt])
matchAlts env scrut alts =
  case scrut of
    MastCon conId args -> findConMatch env (mastIdName conId) args alts
    MastLit lit -> findLitMatch env lit alts
    _ -> Nothing -- Scrutinee not a known value

-- Find a matching constructor alternative
findConMatch :: VEnv -> Name -> [MastExp] -> [MastAlt] -> Maybe (VEnv, [MastStmt])
findConMatch _ _ _ [] = Nothing
findConMatch env conName args ((pat, body) : rest) =
  case pat of
    MastPCon patId pats
      | mastIdName patId == conName ->
          case bindPatterns env pats args of
            Just env' -> Just (env', body)
            Nothing -> findConMatch env conName args rest
    MastPVar varId ->
      let env' = Map.insert varId (MastCon (MastId conName (mastIdType varId)) args) env
       in Just (env', body)
    MastPWildcard -> Just (env, body)
    _ -> findConMatch env conName args rest

-- Find a matching literal alternative
findLitMatch :: VEnv -> Literal -> [MastAlt] -> Maybe (VEnv, [MastStmt])
findLitMatch _ _ [] = Nothing
findLitMatch env lit ((pat, body) : rest) =
  case pat of
    MastPLit patLit | patLit == lit -> Just (env, body)
    MastPVar varId ->
      let env' = Map.insert varId (MastLit lit) env
       in Just (env', body)
    MastPWildcard -> Just (env, body)
    _ -> findLitMatch env lit rest

-- Bind pattern variables to argument expressions
bindPatterns :: VEnv -> [MastPat] -> [MastExp] -> Maybe VEnv
bindPatterns env [] [] = Just env
bindPatterns _ [] _ = Nothing -- Arity mismatch
bindPatterns _ _ [] = Nothing -- Arity mismatch
bindPatterns env (pat : pats) (arg : args) =
  case pat of
    MastPVar varId ->
      let env' =
            if isKnownValue arg
              then Map.insert varId arg env
              else Map.delete varId env
       in bindPatterns env' pats args
    MastPWildcard -> bindPatterns env pats args
    MastPCon patId subPats ->
      case arg of
        MastCon conId subArgs
          | mastIdName conId == mastIdName patId ->
              case bindPatterns env subPats subArgs of
                Just env' -> bindPatterns env' pats args
                Nothing -> Nothing
        _ -> Nothing
    MastPLit patLit ->
      case arg of
        MastLit argLit | argLit == patLit -> bindPatterns env pats args
        _ -> Nothing

-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

-- Check if an expression is a "known value" suitable for inlining/propagation.
-- This includes literals and constructors with all-known arguments.
isKnownValue :: MastExp -> Bool
isKnownValue (MastLit _) = True
isKnownValue (MastCon _ args) = all isKnownValue args
isKnownValue _ = False

-----------------------------------------------------------------------
-- Purity analysis
-----------------------------------------------------------------------

-- Primitives the PE evaluates directly; their std asm bodies are irrelevant
builtinPureFuns :: Set.Set Name
builtinPureFuns =
  Set.fromList
    [ Name "addWord",
      Name "subWord",
      Name "gtWord",
      Name "eqWord",
      Name "concatLit",
      Name "strlenLit",
      Name "keccakLit"
    ]

-- Functions with dummy pure bodies that are intercepted by EmitHull
builtinImpureFuns :: Set.Set Name
builtinImpureFuns = Set.fromList [Name "revert"]

-- | Compute the set of pure functions via fixed-point iteration.
-- Start from builtinPureFuns; each iteration adds functions whose bodies
-- contain no asm and whose every call target is already known-pure.
computePureFuns :: FunTable -> Set.Set Name
computePureFuns ft = go builtinPureFuns
  where
    go pureFuns =
      let pureFuns' =
            Map.foldlWithKey'
              ( \acc fname fd ->
                  if fname `Set.member` acc
                    || fname `Set.member` builtinImpureFuns
                    then acc
                    else
                      if bodyIsPure acc (mastFunBody fd)
                        then Set.insert fname acc
                        else acc
              )
              pureFuns
              ft
       in if Set.size pureFuns' == Set.size pureFuns
            then pureFuns
            else go pureFuns'

bodyIsPure :: Set.Set Name -> [MastStmt] -> Bool
bodyIsPure pureFuns = all (stmtIsPure pureFuns)

stmtIsPure :: Set.Set Name -> MastStmt -> Bool
stmtIsPure _ (MastAsm _) = False
stmtIsPure pureFuns (MastLet _ _ mInit) = maybe True (expIsPure pureFuns) mInit
stmtIsPure pureFuns (MastAssign _ e) = expIsPure pureFuns e
stmtIsPure pureFuns (MastStmtExp e) = expIsPure pureFuns e
stmtIsPure pureFuns (MastReturn e) = expIsPure pureFuns e
stmtIsPure pureFuns (MastMatch e alts) =
  expIsPure pureFuns e && all (bodyIsPure pureFuns . snd) alts

expIsPure :: Set.Set Name -> MastExp -> Bool
expIsPure _ (MastLit _) = True
expIsPure _ (MastVar _) = True
expIsPure pureFuns (MastCall i args) =
  mastIdName i `Set.member` pureFuns && all (expIsPure pureFuns) args
expIsPure pureFuns (MastCon _ args) = all (expIsPure pureFuns) args
expIsPure pureFuns (MastCond e1 e2 e3) =
  expIsPure pureFuns e1 && expIsPure pureFuns e2 && expIsPure pureFuns e3

-----------------------------------------------------------------------
-- Dead code elimination
-----------------------------------------------------------------------

-- | Remove unused functions from a compilation unit.
-- 'start' and 'main' are always considered roots (entry points).
eliminateDeadCode :: MastCompUnit -> MastCompUnit
eliminateDeadCode cu = cu {mastTopDecls = map elimTopDecl (mastTopDecls cu)}
  where
    elimTopDecl (MastTContr c) = MastTContr (elimContract c)
    elimTopDecl d@(MastTDataDef _) = d

    elimContract c = c {mastContrDecls = filter keepDecl (mastContrDecls c)}
      where
        usedNames = findUsedFunctions c
        keepDecl (MastCFunDecl fd) = mastFunName fd `Set.member` usedNames
        keepDecl (MastCMutualDecl ds) =
          -- Keep mutual block if any function in it is used
          any isUsedDecl ds
        keepDecl (MastCDataDecl _) = True

        isUsedDecl (MastCFunDecl fd) = mastFunName fd `Set.member` usedNames
        isUsedDecl (MastCMutualDecl ds) = any isUsedDecl ds
        isUsedDecl (MastCDataDecl _) = True

-- | Find all functions reachable from root functions ('start', 'main')
findUsedFunctions :: MastContract -> Set.Set Name
findUsedFunctions c = go initialRoots initialRoots
  where
    -- Root functions that are always considered used
    rootNames = Set.fromList [Name "start", Name "main"]

    -- Start with roots that actually exist in the contract
    initialRoots = Set.intersection rootNames allFunNames

    -- All function names in the contract
    allFunNames = Set.fromList $ concatMap getFunNames (mastContrDecls c)

    getFunNames (MastCFunDecl fd) = [mastFunName fd]
    getFunNames (MastCMutualDecl ds) = concatMap getFunNames ds
    getFunNames (MastCDataDecl _) = []

    -- Map from function name to its definition
    funTable = Map.fromList $ concatMap getFunDef (mastContrDecls c)

    getFunDef (MastCFunDecl fd) = [(mastFunName fd, fd)]
    getFunDef (MastCMutualDecl ds) = concatMap getFunDef ds
    getFunDef (MastCDataDecl _) = []

    -- Transitive closure: find all reachable functions
    go :: Set.Set Name -> Set.Set Name -> Set.Set Name
    go used worklist
      | Set.null worklist = used
      | otherwise =
          let -- Get calls from all functions in worklist
              newCalls =
                Set.unions
                  [ callsInFun fd | n <- Set.toList worklist, Just fd <- [Map.lookup n funTable]
                  ]
              -- Find newly discovered functions (not yet in used set)
              newFuns = Set.difference newCalls used
              -- Add new functions to used set
              used' = Set.union used newFuns
           in go used' newFuns

-- | Collect all function names called in a function body
callsInFun :: MastFunDef -> Set.Set Name
callsInFun fd = Set.unions (map callsInStmt (mastFunBody fd))

callsInStmt :: MastStmt -> Set.Set Name
callsInStmt (MastLet _ _ mInit) = maybe Set.empty callsInExp mInit
callsInStmt (MastAssign _ e) = callsInExp e
callsInStmt (MastStmtExp e) = callsInExp e
callsInStmt (MastReturn e) = callsInExp e
callsInStmt (MastMatch e alts) =
  Set.union (callsInExp e) (Set.unions [Set.unions (map callsInStmt body) | (_, body) <- alts])
callsInStmt (MastAsm _) = Set.empty

callsInExp :: MastExp -> Set.Set Name
callsInExp (MastLit _) = Set.empty
callsInExp (MastVar _) = Set.empty
callsInExp (MastCall i args) =
  Set.insert (mastIdName i) (Set.unions (map callsInExp args))
callsInExp (MastCon _ args) = Set.unions (map callsInExp args)
callsInExp (MastCond e1 e2 e3) =
  Set.unions [callsInExp e1, callsInExp e2, callsInExp e3]
