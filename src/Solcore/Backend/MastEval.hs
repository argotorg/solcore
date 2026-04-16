module Solcore.Backend.MastEval
  ( evalCompUnit,
    defaultFuel,
    eliminateDeadCode,
    FunTable,
    buildFunTable,
    computePureFuns,
    -- Evaluation monad (exported for testing)
    EvalEnv (..),
    EvalState (..),
    EvalM,
    runEvalM,
    -- Yul interpreter (exported for testing)
    YulState,
    evalYulExp,
    evalYulOp,
    evalYulStmt,
    evalYulBlock,
    asmIsInterpretable,
    maskWord,
    mstoreBytes,
    mloadWord,
  )
where

{- Partial Evaluator for Mast
   Performs compile-time evaluation where possible:
   - Interprets assembly blocks containing supported Yul arithmetic operations
   - Folds calls to `subWord`, `gtWord`, `eqWord` with literal arguments
   - Propagates known variable values
   - Inlines simple pure functions with literal arguments
-}

import Control.Monad.Reader
import Control.Monad.State
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (Keccak_256)
import Data.Bits (shiftR, (.&.))
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Language.Yul (YLiteral (..), YulExp (..), YulStmt (..))
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

-- Type registry: variable name -> MastId (for reconstructing VEnv entries after asm).
-- Pre-scanned from the whole function body so it survives no-init let deletions.
type TypeReg = Map.Map Name MastId

-- | State for Yul arithmetic interpretation.
-- Currently tracks only variable values; may be extended to include memory.
type YulState = Map.Map Name Integer

defaultFuel :: Fuel
defaultFuel = 100

-----------------------------------------------------------------------
-- Evaluation monad
-----------------------------------------------------------------------

data EvalEnv = EvalEnv
  { envFunTable :: FunTable,
    envPureFuns :: Set.Set Name,
    envComptimeMode :: Bool -- True while evaluating a comptime let RHS
  }

data EvalState = EvalState
  { esFuel :: !Fuel,
    esMem :: !(Map.Map Integer Word8)
  }

-- Reader for constant environment, State for fuel + memory
type EvalM = ReaderT EvalEnv (State EvalState)

runEvalM :: EvalEnv -> Fuel -> EvalM a -> (a, Fuel)
runEvalM env fuel m =
  let initState = EvalState {esFuel = fuel, esMem = Map.empty}
      (a, finalState) = runState (runReaderT m env) initState
   in (a, esFuel finalState)

askFunTable :: EvalM FunTable
askFunTable = asks envFunTable

askPureFuns :: EvalM (Set.Set Name)
askPureFuns = asks envPureFuns

askComptimeMode :: EvalM Bool
askComptimeMode = asks envComptimeMode

-- Run an action with comptime mode enabled (memory ops become active).
withComptimeMode :: EvalM a -> EvalM a
withComptimeMode = local (\e -> e {envComptimeMode = True})

getFuel :: EvalM Fuel
getFuel = lift $ gets esFuel

-- Consume one unit of fuel, returns True if fuel was available
useFuel :: EvalM Bool
useFuel = do
  f <- getFuel
  if f > 0
    then do
      lift $ modify (\s -> s {esFuel = esFuel s - 1})
      pure True
    else pure False

-- Restore one unit of fuel (called after successful inlining)
restoreFuel :: EvalM ()
restoreFuel = lift $ modify (\s -> s {esFuel = esFuel s + 1})

getsMem :: EvalM (Map.Map Integer Word8)
getsMem = lift $ gets esMem

modifyMem :: (Map.Map Integer Word8 -> Map.Map Integer Word8) -> EvalM ()
modifyMem f = lift $ modify (\s -> s {esMem = f (esMem s)})

-----------------------------------------------------------------------
-- Main entry point
-----------------------------------------------------------------------

-- Returns the evaluated compilation unit and remaining fuel
evalCompUnit :: Fuel -> MastCompUnit -> (MastCompUnit, Fuel)
evalCompUnit fuel cu = (cu {mastTopDecls = decls'}, remainingFuel)
  where
    funTable = buildFunTable cu
    pureFuns = computePureFuns funTable
    env = EvalEnv {envFunTable = funTable, envPureFuns = pureFuns, envComptimeMode = False}
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
-- Type registry: pre-scan a function body for all declared MastIds
-----------------------------------------------------------------------

-- | Build a registry from variable names to their full MastIds.
-- Covers function parameters and let-declared variables throughout the body.
-- Used to reconstruct VEnv entries after interpreting an asm block, since
-- a no-init 'let' deletes the variable from VEnv before the asm block runs.
buildTypeReg :: [MastParam] -> [MastStmt] -> TypeReg
buildTypeReg params stmts =
  Map.fromList $
    [(mastParamName p, MastId (mastParamName p) (mastParamType p)) | p <- params]
      ++ concatMap letIds stmts
  where
    letIds (MastLet _ i _ _) = [(mastIdName i, i)]
    letIds (MastMatch _ alts) = concatMap (concatMap letIds . snd) alts
    letIds _ = []

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
  modifyMem (const Map.empty)
  let tyReg = buildTypeReg (mastFunParams fd) (mastFunBody fd)
  (_, body') <- evalStmts tyReg Map.empty (mastFunBody fd)
  pure $ fd {mastFunBody = body'}

-----------------------------------------------------------------------
-- Evaluate statements (AST transformation)
-----------------------------------------------------------------------

-- Transform statements in place, evaluating expressions where possible.
-- Used for optimizing function bodies that remain in the output.
-- Compare with evalFunBody which extracts a return value for inlining.
-- TODO: consider unifying these two statement handlers

-- Process statements left-to-right, threading environment through
evalStmts :: TypeReg -> VEnv -> [MastStmt] -> EvalM (VEnv, [MastStmt])
evalStmts _ env [] = pure (env, [])
evalStmts tyReg env (s : ss) = do
  (env', s') <- evalStmt tyReg env s
  (env'', ss') <- evalStmts tyReg env' ss
  pure (env'', s' <> ss')

evalStmt :: TypeReg -> VEnv -> MastStmt -> EvalM (VEnv, [MastStmt])
evalStmt tyReg env stmt = case stmt of
  MastLet ct i ty mInit -> do
    mInit' <- traverse (if ct then withComptimeMode . evalExp env else evalExp env) mInit
    let env' = case mInit' of
          Just e | isKnownValue e -> Map.insert i e env
          _ -> Map.delete i env -- Shadow/remove any existing binding
          -- Comptime lets with known values are dead after evaluation: all uses will be
          -- substituted from VEnv, and EmitHull cannot handle string-typed variables.
    let stmts = case mInit' of
          Just e | ct && isKnownValue e -> []
          _ -> [MastLet ct i ty mInit']
    pure (env', stmts)
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
    alts' <- mapM (evalAlt tyReg env) alts
    pure (env, [MastMatch e' alts'])
  MastAsm yul -> do
    -- Substitute known comptime values into the block first.  This is necessary
    -- because comptime lets with known values are eliminated from the statement
    -- list; any asm reference to those variables must be replaced inline.
    let subst = venvToSubst env
        yul' = substYulBlock subst yul
    -- Try to interpret arithmetic-only asm blocks statically.
    -- If successful, update VEnv with computed values (but still emit the block
    -- for code generation). If not, conservatively clear all bindings.
    let yulState = venvToYulState env
    mYulState' <- evalYulBlock yulState yul'
    case mYulState' of
      Just yulState' ->
        pure (mergeYulStateToVEnv tyReg yulState' env, [MastAsm yul'])
      Nothing ->
        pure (Map.empty, [MastAsm yul'])

evalAlt :: TypeReg -> VEnv -> MastAlt -> EvalM MastAlt
evalAlt tyReg env (pat, body) = do
  -- Pattern bindings shadow existing bindings, but we don't track them
  -- (conservative: treat all pattern-bound vars as unknown)
  pat' <- evalPat env pat
  (_, body') <- evalStmts tyReg env body
  pure (pat', body')

-- Evaluate expression labels in patterns.
-- MastPExp must reduce to a literal; any other form is a compile-time error.
evalPat :: VEnv -> MastPat -> EvalM MastPat
evalPat env (MastPExp e) = do
  e' <- evalExp env e
  case e' of
    MastLit l -> pure (MastPLit l)
    _ -> error $ "comptime expression in match label could not be evaluated to a literal: " ++ show e'
evalPat _ pat = pure pat

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
-- Primitive evaluation (named-function fast paths)
-----------------------------------------------------------------------

evalPrimitive :: Name -> [MastExp] -> Maybe MastExp
evalPrimitive (Name "subWord") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just (MastLit (IntLit (maskWord (a - b))))
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
-- Yul interpreter: memory helpers (pure)
-----------------------------------------------------------------------

-- | 256-bit word modulus (EVM semantics for arithmetic operations).
wordMod :: Integer
wordMod = 2 ^ (256 :: Integer)

-- | Truncate to 256-bit unsigned word.
maskWord :: Integer -> Integer
maskWord n = n `mod` wordMod

-- | Write a 256-bit value to memory at byte address p (big-endian, 32 bytes).
mstoreBytes :: Integer -> Integer -> Map.Map Integer Word8 -> Map.Map Integer Word8
mstoreBytes p v mem =
  foldl'
    (\m i -> Map.insert (p + i) (fromIntegral ((v `shiftR` (8 * (31 - fromIntegral i))) .&. 0xff)) m)
    mem
    [0 .. 31]

-- | Read a 256-bit value from memory at byte address p (big-endian, 32 bytes).
-- Returns Nothing if any byte in p..p+31 was not written during this comptime
-- evaluation. We cannot assume unwritten bytes are 0: runtime code may have
-- written to memory before this function executes (e.g. the free memory pointer
-- at slot 64 is set by initialization code before any user function runs).
mloadWord :: Integer -> Map.Map Integer Word8 -> Maybe Integer
mloadWord p mem =
  foldl'
    (\mAcc i -> do acc <- mAcc; b <- Map.lookup (p + i) mem; pure (acc * 256 + fromIntegral b))
    (Just 0)
    [0 .. 31]

-----------------------------------------------------------------------
-- Yul interpreter: expression and statement evaluator (in EvalM)
-----------------------------------------------------------------------

-- | Evaluate a Yul expression given the current Yul state.
-- Returns Nothing if any operand is unknown or the operation is unsupported.
-- Clause order matters: mload must precede the general YCall catch-all.
evalYulExp :: YulState -> YulExp -> EvalM (Maybe Integer)
evalYulExp env (YIdent n) = pure (Map.lookup n env)
evalYulExp _ (YLit (YulNumber n)) = pure (Just n)
evalYulExp _ (YLit YulTrue) = pure (Just 1)
evalYulExp _ (YLit YulFalse) = pure (Just 0)
evalYulExp env (YCall (Name "mload") [pExp]) = do
  compt <- askComptimeMode
  if not compt
    then pure Nothing -- mload is only meaningful in comptime context
    else do
      mp <- evalYulExp env pExp
      case mp of
        Just p -> mloadWord p <$> getsMem -- Nothing if any byte unwritten
        Nothing -> pure Nothing
evalYulExp env (YCall op args) = do
  mvals <- mapM (evalYulExp env) args
  case sequence mvals of
    Nothing -> pure Nothing
    Just vals -> evalYulOp op vals
evalYulExp _ _ = pure Nothing

-- | Evaluate a Yul built-in operation on known integer values.
-- Returns Nothing for unsupported or unknown operations.
evalYulOp :: Name -> [Integer] -> EvalM (Maybe Integer)
evalYulOp (Name "add") [a, b] = pure (Just (maskWord (a + b)))
evalYulOp (Name "mul") [a, b] = pure (Just (maskWord (a * b)))
evalYulOp _ _ = pure Nothing

-- | Evaluate one Yul statement, updating the Yul state.
-- mstore/mstore8 update EvalM memory; assignments update YulState.
-- Returns Nothing if the statement form is unsupported.
evalYulStmt :: YulState -> YulStmt -> EvalM (Maybe YulState)
evalYulStmt env (YAssign [n] e) = do
  mv <- evalYulExp env e
  pure (fmap (\v -> Map.insert n v env) mv)
evalYulStmt env (YExp (YCall (Name "mstore") [pExp, vExp])) = do
  compt <- askComptimeMode
  if not compt
    then pure Nothing -- mstore only in comptime context; fail block so callers aren't inlined
    else do
      mp <- evalYulExp env pExp
      mv <- evalYulExp env vExp
      case (mp, mv) of
        (Just p, Just v) -> do
          modifyMem (mstoreBytes p v)
          pure (Just env) -- YulState unchanged; memory updated in EvalM
        _ -> pure Nothing
evalYulStmt env (YExp (YCall (Name "mstore8") [pExp, vExp])) = do
  compt <- askComptimeMode
  if not compt
    then pure Nothing -- mstore8 only in comptime context
    else do
      mp <- evalYulExp env pExp
      mv <- evalYulExp env vExp
      case (mp, mv) of
        (Just p, Just v) -> do
          modifyMem (Map.insert p (fromIntegral (v .&. 0xff)))
          pure (Just env)
        _ -> pure Nothing
evalYulStmt _ _ = pure Nothing

-- | Evaluate a Yul block, threading the Yul state through each statement.
-- Returns Nothing if any statement cannot be evaluated.
-- mstore/mstore8/mload only execute in comptime mode; outside it they return
-- Nothing immediately, aborting the block and preventing unsound inlining.
evalYulBlock :: YulState -> [YulStmt] -> EvalM (Maybe YulState)
evalYulBlock env [] = pure (Just env)
evalYulBlock env (s : ss) = do
  menv' <- evalYulStmt env s
  case menv' of
    Nothing -> pure Nothing
    Just env' -> evalYulBlock env' ss

-----------------------------------------------------------------------
-- VEnv / YulState / TypeReg helpers
-----------------------------------------------------------------------

-- | Extract known integer values from a VEnv into a YulState.
venvToYulState :: VEnv -> YulState
venvToYulState env =
  Map.fromList [(mastIdName k, v) | (k, MastLit (IntLit v)) <- Map.toList env]

-- | Build a name→YulExp substitution map from all known literal values in VEnv.
-- Used to inline comptime values into asm blocks so that eliminated 'let' bindings
-- remain available to the asm code.
venvToSubst :: VEnv -> Map.Map Name YulExp
venvToSubst env =
  Map.fromList
    [ (mastIdName k, yulLit l)
      | (k, MastLit l) <- Map.toList env
    ]
  where
    yulLit (IntLit v) = YLit (YulNumber v)
    yulLit (StrLit s) = YLit (YulString s)

-- | Substitute known literal values into a Yul block.
-- Only replaces YIdent occurrences in expression positions; does not touch
-- variable names on the left-hand side of let/assign or function parameter lists.
substYulBlock :: Map.Map Name YulExp -> [YulStmt] -> [YulStmt]
substYulBlock subst = map (substYulStmt subst)

substYulStmt :: Map.Map Name YulExp -> YulStmt -> YulStmt
substYulStmt subst (YAssign names e) = YAssign names (substYulExp subst e)
substYulStmt subst (YExp e) = YExp (substYulExp subst e)
substYulStmt subst (YLet names me) = YLet names (fmap (substYulExp subst) me)
substYulStmt subst (YIf e block) = YIf (substYulExp subst e) (substYulBlock subst block)
substYulStmt subst (YBlock stmts) = YBlock (substYulBlock subst stmts)
substYulStmt subst (YFun n as rs body) = YFun n as rs (substYulBlock subst body)
substYulStmt subst (YFor pre c post b) =
  YFor
    (substYulBlock subst pre)
    (substYulExp subst c)
    (substYulBlock subst post)
    (substYulBlock subst b)
substYulStmt subst (YSwitch e cases def) =
  YSwitch
    (substYulExp subst e)
    (map (\(lit, block) -> (lit, substYulBlock subst block)) cases)
    (fmap (substYulBlock subst) def)
substYulStmt _ stmt = stmt -- YBreak, YContinue, YLeave, YComment unchanged

substYulExp :: Map.Map Name YulExp -> YulExp -> YulExp
substYulExp subst (YIdent n) = Map.findWithDefault (YIdent n) n subst
substYulExp subst (YCall op args) = YCall op (map (substYulExp subst) args)
substYulExp _ e = e -- YLit, YMeta unchanged

-- | Merge a YulState back into VEnv, using TypeReg to find the right MastIds.
-- Only names present in TypeReg are merged; others are silently ignored.
mergeYulStateToVEnv :: TypeReg -> YulState -> VEnv -> VEnv
mergeYulStateToVEnv tyReg yulState venv =
  Map.foldlWithKey' update venv yulState
  where
    update acc n v =
      case Map.lookup n tyReg of
        Just mastId -> Map.insert mastId (MastLit (IntLit v)) acc
        Nothing -> acc

-----------------------------------------------------------------------
-- Function inlining
-----------------------------------------------------------------------

-- Try to inline a function call.
-- Works when: (1) all arguments are known values, or
--             (2) function is "constant" (ignores its arguments)
-- Only pure functions (no non-interpretable asm, no impure calls) are eligible.
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
                  tyReg = buildTypeReg params (mastFunBody fd)
              evalFunBody tyReg env (mastFunBody fd)

-- Evaluate a function body and extract the return value
evalFunBody :: TypeReg -> VEnv -> [MastStmt] -> EvalM (Maybe MastExp)
evalFunBody _ _ [] = pure Nothing -- No return statement found
evalFunBody tyReg env (stmt : rest) = case stmt of
  MastLet _ i _ mInit -> do
    mInit' <- traverse (evalExp env) mInit
    let env' = case mInit' of
          Just e | isKnownValue e -> Map.insert i e env
          _ -> Map.delete i env
    evalFunBody tyReg env' rest
  MastAssign i e -> do
    e' <- evalExp env e
    let env' =
          if isKnownValue e'
            then Map.insert i e' env
            else Map.delete i env
    evalFunBody tyReg env' rest
  MastStmtExp _ -> evalFunBody tyReg env rest
  MastReturn e -> do
    e' <- evalExp env e
    pure $ if isKnownValue e' then Just e' else Nothing
  MastMatch scrut alts -> do
    scrut' <- evalExp env scrut
    alts' <- mapM (\(p, b) -> (,b) <$> evalPat env p) alts
    case matchAlts env scrut' alts' of
      Just (env', body) -> evalFunBody tyReg env' body
      Nothing -> pure Nothing -- Scrutinee not known, can't select branch
  MastAsm yul -> do
    -- Try to interpret the asm block statically.
    -- If successful, update env and continue inlining; otherwise give up.
    let yulState = venvToYulState env
    mYulState' <- evalYulBlock yulState yul
    case mYulState' of
      Just yulState' -> evalFunBody tyReg (mergeYulStateToVEnv tyReg yulState' env) rest
      Nothing -> pure Nothing

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
    MastPExp _ -> error "PANIC: MastPExp reached findLitMatch — evalAlt failed to evaluate it"
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
    MastPExp _ -> error "PANIC: MastPExp reached bindPatterns — evalAlt failed to evaluate it"

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
    [ Name "subWord",
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
-- Self-recursive calls are handled by including the candidate function in
-- the assumed-pure set when checking its own body.
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
                      if bodyIsPure (Set.insert fname acc) (mastFunBody fd)
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
-- Asm blocks are pure only if every statement uses a statically interpretable
-- operation. This keeps storage/memory reads (sload, mload, …) out of pureFuns,
-- which is load-bearing for the MAST-level comptime check.
stmtIsPure _ (MastAsm stmts) = asmIsInterpretable stmts
stmtIsPure pureFuns (MastLet _ _ _ mInit) = maybe True (expIsPure pureFuns) mInit
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

-- | True if an asm block contains only statically interpretable statements.
-- Such blocks can be evaluated at compile time by the Yul interpreter,
-- and functions containing only such blocks are eligible for PE inlining.
-- Operations NOT listed here (sload, …) make the block non-interpretable,
-- which keeps the enclosing function out of pureFuns and preserves comptime-check soundness.
asmIsInterpretable :: [YulStmt] -> Bool
asmIsInterpretable = all interpretableStmt
  where
    interpretableStmt (YAssign [_] e) = interpretableExp e
    interpretableStmt (YExp (YCall (Name "mstore") [p, v])) = interpretableExp p && interpretableExp v
    interpretableStmt (YExp (YCall (Name "mstore8") [p, v])) = interpretableExp p && interpretableExp v
    interpretableStmt _ = False

    -- mload has its own clause (reads memory); general YCall delegates to interpretableOp
    interpretableExp (YIdent _) = True
    interpretableExp (YLit (YulNumber _)) = True
    interpretableExp (YLit YulTrue) = True
    interpretableExp (YLit YulFalse) = True
    interpretableExp (YCall (Name "mload") [p]) = interpretableExp p
    interpretableExp (YCall op args) =
      interpretableOp op (length args) && all interpretableExp args
    interpretableExp _ = False

    interpretableOp (Name "add") 2 = True
    interpretableOp (Name "mul") 2 = True
    interpretableOp _ _ = False

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
callsInStmt (MastLet _ _ _ mInit) = maybe Set.empty callsInExp mInit
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
