module Solcore.Backend.MastEval
  ( evalCompUnit
  , defaultFuel
  ) where

{- Partial Evaluator for Mast
   Performs compile-time evaluation where possible:
   - Folds calls to `addWord`, `gtWord`, `eqWord` with literal arguments
   - Propagates known variable values
   - Inlines simple pure functions with literal arguments
-}

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Solcore.Backend.Mast
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt(Literal(..))

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

-- Reader for constant function table, State for fuel budget
type EvalM = ReaderT FunTable (State Fuel)

runEvalM :: FunTable -> Fuel -> EvalM a -> (a, Fuel)
runEvalM ft fuel m = runState (runReaderT m ft) fuel

askFunTable :: EvalM FunTable
askFunTable = ask

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
restoreFuel = lift $ modify (+1)

-----------------------------------------------------------------------
-- Main entry point
-----------------------------------------------------------------------

-- Returns the evaluated compilation unit and remaining fuel
evalCompUnit :: Fuel -> MastCompUnit -> (MastCompUnit, Fuel)
evalCompUnit fuel cu = (cu { mastTopDecls = decls' }, remainingFuel)
  where
    funTable = buildFunTable cu
    (decls', remainingFuel) = runEvalM funTable fuel (mapM evalTopDecl (mastTopDecls cu))

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
  pure $ c { mastContrDecls = decls' }

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
  pure $ fd { mastFunBody = body' }

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
evalStmts env (s:ss) = do
  (env', s') <- evalStmt env s
  (env'', ss') <- evalStmts env' ss
  pure (env'', s' : ss')

evalStmt :: VEnv -> MastStmt -> EvalM (VEnv, MastStmt)
evalStmt env stmt = case stmt of
  MastLet i ty mInit -> do
    mInit' <- traverse (evalExp env) mInit
    let env' = case mInit' of
          Just e | isKnownValue e -> Map.insert i e env
          _ -> Map.delete i env  -- Shadow/remove any existing binding
    pure (env', MastLet i ty mInit')

  MastAssign i e -> do
    e' <- evalExp env e
    let env' = if isKnownValue e'
               then Map.insert i e' env
               else Map.delete i env  -- Value no longer known
    pure (env', MastAssign i e')

  MastStmtExp e -> do
    e' <- evalExp env e
    pure (env, MastStmtExp e')

  MastReturn e -> do
    e' <- evalExp env e
    pure (env, MastReturn e')

  MastMatch e alts -> do
    e' <- evalExp env e
    alts' <- mapM (evalAlt env) alts
    pure (env, MastMatch e' alts')

  MastAsm yul ->
    -- Assembly blocks are opaque; we don't know what they modify
    -- Conservative: clear all variable bindings
    pure (Map.empty, MastAsm yul)

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
          restoreFuel  -- Restore fuel: it acts purely as recursion depth limit
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
evalPrimitive _ _ = Nothing

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
tryInline :: Name -> [MastExp] -> EvalM (Maybe MastExp)
tryInline fname args = do
  ft <- askFunTable
  case Map.lookup fname ft of
    Nothing -> pure Nothing
    Just fd
      | not (isInlinable fd) -> pure Nothing
      | length (mastFunParams fd) /= length args -> pure Nothing
      | otherwise -> do
          let params = mastFunParams fd
              paramToId p = MastId (mastParamName p) (mastParamType p)
              env = Map.fromList $ zip (map paramToId params) args
          evalFunBody env (mastFunBody fd)

-- Check if a function can be inlined
isInlinable :: MastFunDef -> Bool
isInlinable fd = all isInlinableStmt (mastFunBody fd)
  where
    isInlinableStmt (MastAsm _) = False
    isInlinableStmt (MastLet _ _ _) = True
    isInlinableStmt (MastAssign _ _) = True
    isInlinableStmt (MastStmtExp _) = True
    isInlinableStmt (MastReturn _) = True
    isInlinableStmt (MastMatch _ alts) = all (all isInlinableStmt . snd) alts

-- Evaluate a function body and extract the return value
evalFunBody :: VEnv -> [MastStmt] -> EvalM (Maybe MastExp)
evalFunBody _ [] = pure Nothing  -- No return statement found
evalFunBody env (stmt:rest) = case stmt of
  MastLet i _ mInit -> do
    mInit' <- traverse (evalExp env) mInit
    let env' = case mInit' of
          Just e | isKnownValue e -> Map.insert i e env
          _ -> Map.delete i env
    evalFunBody env' rest

  MastAssign i e -> do
    e' <- evalExp env e
    let env' = if isKnownValue e'
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
      Nothing -> pure Nothing  -- Scrutinee not known, can't select branch

  MastAsm _ -> pure Nothing  -- Should not happen due to isInlinable check

-- Try to match a known scrutinee against alternatives.
-- Returns the extended environment and the body of the matching alternative.
matchAlts :: VEnv -> MastExp -> [MastAlt] -> Maybe (VEnv, [MastStmt])
matchAlts env scrut alts =
  case scrut of
    MastCon conId args -> findConMatch env (mastIdName conId) args alts
    MastLit lit -> findLitMatch env lit alts
    _ -> Nothing  -- Scrutinee not a known value

-- Find a matching constructor alternative
findConMatch :: VEnv -> Name -> [MastExp] -> [MastAlt] -> Maybe (VEnv, [MastStmt])
findConMatch _ _ _ [] = Nothing
findConMatch env conName args ((pat, body):rest) =
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
findLitMatch env lit ((pat, body):rest) =
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
bindPatterns _ [] _ = Nothing  -- Arity mismatch
bindPatterns _ _ [] = Nothing  -- Arity mismatch
bindPatterns env (pat:pats) (arg:args) =
  case pat of
    MastPVar varId ->
      let env' = if isKnownValue arg
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
