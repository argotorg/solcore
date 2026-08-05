module Solcore.Backend.MastEval
  ( evalCompUnit,
    defaultFuel,
    eliminateDeadCode,
    FunTable,
    buildFunTable,
    computePureFuns,
    isKnownValue,
    -- Evaluation monad (exported for testing)
    EvalEnv (..),
    EvalState (..),
    EvalM,
    runEvalM,
    -- Yul interpreter (exported for testing)
    YulVal (..),
    MemRegion (..),
    MemAddr,
    memAddr,
    YulState,
    evalYulExp,
    evalYulOp,
    evalYulStmt,
    evalYulBlock,
    asmIsInterpretable,
    maskWord,
    mstoreBytes,
    mloadWord,
    mloadRange,
    -- Primitive evaluator (exported for testing)
    evalPrimitive,
  )
where

{- Partial Evaluator for Mast
   Performs compile-time evaluation where possible:
   - Interprets assembly blocks containing supported Yul arithmetic operations
   - Folds calls to `subWord`, `gtWord`, `bxorWord`, `bandWord`, `borWord`, `eqWord` with literal arguments
   - Propagates known variable values
   - Inlines simple pure functions with literal arguments
-}

import Control.Applicative ((<|>))
import Control.Monad.Reader
import Control.Monad.State
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (Keccak_256)
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Traversable (mapAccumM)
import Data.Word (Word8)
import Language.Yul (YLiteral (..), YulExp (..), YulStmt (..))
import Solcore.Backend.Mast
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt (Literal (..))
import Solcore.Primitives.Primitives (integerPrimNames, memStringFromLitName, stringPrimNames)

-----------------------------------------------------------------------
-- Data structures
-----------------------------------------------------------------------

-- | The outcome of partially evaluating a MAST expression: the expression to
-- emit, plus its offset from the free memory pointer when the value is a
-- compile-time address.  Such an address has no MAST form — its base is only
-- known at run time — so it travels beside 'resExp' rather than inside it.
-- That way it can drive the comptime memory model without any risk of being
-- folded into the emitted program.
data EvalResult = EvalResult
  { resExp :: MastExp,
    resFreeOffset :: Maybe Integer
  }

plainResult :: MastExp -> EvalResult
plainResult e = EvalResult {resExp = e, resFreeOffset = Nothing}

-- | True if the value may be propagated through the environment: a literal,
-- a constructor of such, or a symbolic address.
resIsKnown :: EvalResult -> Bool
resIsKnown r = isJust (resFreeOffset r) || isKnownValue (resExp r)

-- | True if the value may replace its expression in the emitted program.
-- A symbolic address may not: there is no MAST expression denoting it.
resIsFoldable :: EvalResult -> Bool
resIsFoldable r = isNothing (resFreeOffset r) && isKnownValue (resExp r)

-- Variable environment: variable id (name + type) -> known value
-- Uses full MastId to distinguish variables with same name but different types
type VEnv = Map.Map MastId EvalResult

-- | Record a variable's value, or forget it if the value is not known.
-- A symbolic address is rebound to the variable itself, so that later uses emit
-- the variable rather than re-running the expression that produced the address.
bindResult :: MastId -> EvalResult -> VEnv -> VEnv
bindResult i r env = case resFreeOffset r of
  Just k -> Map.insert i (EvalResult (MastVar i) (Just k)) env
  Nothing
    | isKnownValue (resExp r) -> Map.insert i r env
    | otherwise -> Map.delete i env

-- Function table: function name -> definition
type FunTable = Map.Map Name MastFunDef

-- Fuel for controlling recursion depth during inlining
type Fuel = Int

-- Type registry: variable name -> MastId (for reconstructing VEnv entries after asm).
-- Pre-scanned from the whole function body so it survives no-init let deletions.
type TypeReg = Map.Map Name MastId

-- | A value in the Yul interpreter.  Most values are concrete 256-bit words,
-- but reading the free memory pointer yields an address whose base is unknown
-- at compile time.  Tracking it symbolically, as an offset from that base, lets
-- comptime code borrow scratch space above the free pointer.
data YulVal
  = Concrete Integer
  | FreeOffset Integer
  deriving (Eq, Show)

-- | Comptime memory is split into two regions: absolute addresses, and
-- addresses relative to the free memory pointer.  Whether they overlap depends
-- on the run-time value of that pointer, so a single comptime evaluation may
-- only use one of them (see 'regionUsable').
data MemRegion = AbsoluteMem | FreeMem
  deriving (Eq, Ord, Show)

type MemAddr = (MemRegion, Integer)

-- | The memory address denoted by a Yul value.
memAddr :: YulVal -> MemAddr
memAddr (Concrete n) = (AbsoluteMem, n)
memAddr (FreeOffset k) = (FreeMem, k)

-- | State for Yul arithmetic interpretation: local Yul variable bindings.
type YulState = Map.Map Name YulVal

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

-- Identifies a clone of a function: the callee, plus the literal bound to each
-- of its comptime-only parameters.  Two call sites passing the same literals
-- share one clone.
type CloneKey = (Name, [(Name, Literal)])

data EvalState = EvalState
  { esFuel :: !Fuel,
    esMem :: !(Map.Map MemAddr Word8),
    -- clones created so far, for reuse across call sites
    esCloneNames :: !(Map.Map CloneKey Name),
    -- clone definitions, by clone name; kept for clone-of-clone lookups
    esCloneDefs :: !(Map.Map Name MastFunDef),
    -- clones created but not yet evaluated, with the environment binding their
    -- erased parameters
    esPendingClones :: ![(MastFunDef, VEnv)]
  }

-- Reader for constant environment, State for fuel + memory
type EvalM = ReaderT EvalEnv (State EvalState)

runEvalM :: EvalEnv -> Fuel -> EvalM a -> (a, Fuel)
runEvalM env fuel m =
  let initState =
        EvalState
          { esFuel = fuel,
            esMem = Map.empty,
            esCloneNames = Map.empty,
            esCloneDefs = Map.empty,
            esPendingClones = []
          }
      (a, finalState) = runState (runReaderT m env) initState
   in (a, esFuel finalState)

askFunTable :: EvalM FunTable
askFunTable = asks envFunTable

askPureFuns :: EvalM (Set.Set Name)
askPureFuns = asks envPureFuns

askComptimeMode :: EvalM Bool
askComptimeMode = asks envComptimeMode

-- Run an action with comptime mode enabled (memory ops become active).
-- Entering comptime mode starts from fresh memory: each comptime evaluation is
-- an independent hypothetical execution and must not observe another's writes.
-- A nested entry (a '-> comptime' call inside a comptime let) keeps the memory,
-- since it is part of the same evaluation.
withComptimeMode :: EvalM a -> EvalM a
withComptimeMode m = do
  nested <- askComptimeMode
  if nested then pure () else modifyMem (const Map.empty)
  local (\e -> e {envComptimeMode = True}) m

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

getsMem :: EvalM (Map.Map MemAddr Word8)
getsMem = lift $ gets esMem

modifyMem :: (Map.Map MemAddr Word8 -> Map.Map MemAddr Word8) -> EvalM ()
modifyMem f = lift $ modify (\s -> s {esMem = f (esMem s)})

-- | True if a memory access in this region is compatible with what the current
-- comptime evaluation has already written.  Absolute and free-pointer-relative
-- addresses may alias at run time, so mixing the two regions within one
-- evaluation could silently produce a wrong answer; the evaluator gives up.
regionUsable :: MemRegion -> EvalM Bool
regionUsable r = do
  mem <- getsMem
  pure $ case Map.lookupMin mem of
    Nothing -> True
    Just ((r', _), _) -> r' == r

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
  -- Clones are emitted into the contract that needed them, so a clone from an
  -- earlier contract must not be reused here: it lives in a different Yul
  -- object.  Names stay globally unique (esCloneDefs is never reset), so the
  -- two contracts get distinct copies rather than a dangling call.
  lift $ modify (\s -> s {esCloneNames = Map.empty})
  decls' <- mapM evalContractDecl (mastContrDecls c)
  clones <- drainClones
  pure $ c {mastContrDecls = decls' ++ map MastCFunDecl clones}

-- Evaluate the clones queued while the contract was evaluated, and any queued
-- in turn by those.  Each clone is emitted into the contract whose code
-- created it, so it lands in the same Yul object as its caller.
drainClones :: EvalM [MastFunDef]
drainClones = go []
  where
    go acc = do
      pending <- lift $ gets esPendingClones
      if null pending
        then pure (reverse acc)
        else do
          lift $ modify (\s -> s {esPendingClones = []})
          evaluated <- mapM evalCloneDef (reverse pending)
          go (reverse evaluated ++ acc)

-- Like `evalFunDef`, but the erased parameters start out bound to their
-- literals, which is what substitutes them into the body.
evalCloneDef :: (MastFunDef, VEnv) -> EvalM MastFunDef
evalCloneDef (fd, env0) = do
  modifyMem (const Map.empty)
  let tyReg = buildTypeReg (mastFunParams fd) (mastFunBody fd)
  (_, body') <- evalStmts tyReg env0 (mastFunBody fd)
  let fd' = fd {mastFunBody = body'}
  lift $ modify (\s -> s {esCloneDefs = Map.insert (mastFunName fd) fd' (esCloneDefs s)})
  pure fd'

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
          Just r -> bindResult i r env
          Nothing -> Map.delete i env -- Shadow/remove any existing binding
          -- Comptime lets with known values are dead after evaluation: all uses will be
          -- substituted from VEnv, and EmitHull cannot handle string-typed variables.
    let stmts = case mInit' of
          Just r | ct && resIsFoldable r -> []
          _ -> [MastLet ct i ty (resExp <$> mInit')]
    pure (env', stmts)
  MastAssign i e -> do
    r <- evalExp env e
    -- Always emit the assignment: the variable may be referenced by opaque asm blocks
    pure (bindResult i r env, [MastAssign i (resExp r)])
  MastStmtExp e -> do
    r <- evalExp env e
    if resIsFoldable r
      then pure (env, [])
      else pure (env, [MastStmtExp (resExp r)])
  MastReturn e -> do
    r <- evalExp env e
    pure (env, [MastReturn (resExp r)])
  MastMatch e alts -> do
    e' <- resExp <$> evalExp env e
    alts' <- mapM (evalAlt tyReg env) alts
    -- Any variable assigned in any alt may be updated; remove from env
    -- so downstream code doesn't see the pre-match value.
    let mutated = foldMap (assignedInStmts . snd) alts
        env' = foldr Map.delete env (Set.toList mutated)
    pure (env', [MastMatch e' alts'])
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
  MastSeq stmts -> evalStmts tyReg env stmts
  MastBreak -> pure (env, [MastBreak])
  MastContinue -> pure (env, [MastContinue])
  MastFor initStmt cond post body -> do
    -- Evaluate loop parts for local simplification, but do not propagate
    -- value bindings across the loop boundary.
    -- Remove any variables assigned in the loop from the environment to
    -- prevent stale pre-loop values from being constant-propagated into
    -- the loop body (e.g. `s := 0` before the loop must not replace `s`
    -- inside the body where `s` is being updated).
    let assigned =
          assignedInStmt initStmt
            `Set.union` assignedInStmt post
            `Set.union` foldMap assignedInStmt body
        loopEnv = foldr Map.delete env (Set.toList assigned)
    (_, initStmt') <- evalLoopStmt loopEnv initStmt
    cond' <- resExp <$> evalExp loopEnv cond
    (_, post') <- evalLoopStmt loopEnv post
    (_, bodies') <- mapAccumM evalLoopStmt loopEnv body
    pure (Map.empty, [MastFor initStmt' cond' post' bodies'])

-- Evaluate a statement while preserving statement shape.
-- Used for nested contexts like MastFor where we cannot drop statements.
evalLoopStmt :: VEnv -> MastStmt -> EvalM (VEnv, MastStmt)
evalLoopStmt env st = case st of
  MastLet ct i ty mInit -> do
    mInit' <- traverse (evalExp env) mInit
    let env' = case mInit' of
          Just r -> bindResult i r env
          Nothing -> Map.delete i env
    pure (env', MastLet ct i ty (resExp <$> mInit'))
  MastAssign i e -> do
    r <- evalExp env e
    pure (bindResult i r env, MastAssign i (resExp r))
  MastStmtExp e -> do
    e' <- resExp <$> evalExp env e
    pure (env, MastStmtExp e')
  MastReturn e -> do
    e' <- resExp <$> evalExp env e
    pure (env, MastReturn e')
  MastMatch e alts -> do
    e' <- resExp <$> evalExp env e
    -- No tyReg in loop context; asm in loop bodies is treated as opaque.
    alts' <- mapM (evalAlt Map.empty env) alts
    let mutated = foldMap (assignedInStmts . snd) alts
        env' = foldr Map.delete env (Set.toList mutated)
    pure (env', MastMatch e' alts')
  MastFor initStmt cond post body -> do
    (_, initStmt') <- evalLoopStmt env initStmt
    cond' <- resExp <$> evalExp env cond
    (_, post') <- evalLoopStmt env post
    bodies' <- mapM (fmap snd . evalLoopStmt env) body
    pure (Map.empty, MastFor initStmt' cond' post' bodies')
  MastAsm yul -> pure (Map.empty, MastAsm yul)
  MastBreak -> pure (env, MastBreak)
  MastContinue -> pure (env, MastContinue)
  MastSeq stmts -> do
    (env', stmts') <- mapAccumM evalLoopStmt env stmts
    pure (env', MastSeq stmts')

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
  e' <- resExp <$> evalExp env e
  case e' of
    MastLit l -> pure (MastPLit l)
    _ -> error $ "comptime expression in match label could not be evaluated to a literal: " ++ show e'
evalPat _ pat = pure pat

-- Collect variables assigned (via MastAssign) in a list of statements.
-- Recurses into nested match arms. Used to invalidate env entries after a match.
assignedInStmts :: [MastStmt] -> Set.Set MastId
assignedInStmts = foldMap assignedInStmt

assignedInStmt :: MastStmt -> Set.Set MastId
assignedInStmt (MastAssign i _) = Set.singleton i
assignedInStmt (MastMatch _ alts) = foldMap (assignedInStmts . snd) alts
assignedInStmt (MastFor initStmt _ post body) =
  assignedInStmt initStmt
    `Set.union` assignedInStmt post
    `Set.union` foldMap assignedInStmt body
assignedInStmt (MastSeq stmts) = foldMap assignedInStmt stmts
assignedInStmt _ = Set.empty

-----------------------------------------------------------------------
-- Evaluate expressions
-----------------------------------------------------------------------

evalExp :: VEnv -> MastExp -> EvalM EvalResult
evalExp _ expr@(MastLit _) = pure (plainResult expr)
evalExp env expr@(MastVar i) =
  pure $ case Map.lookup i env of
    Just r -> r
    Nothing -> plainResult expr
evalExp env (MastCall i args) = do
  args' <- mapM (evalExp env) args
  let fname = mastIdName i
      residual = MastCall i (map resExp args')
  case evalPrimitive fname (map resExp args') of
    Just result -> pure (plainResult result)
    Nothing -> do
      -- Try inlining if we have fuel
      hasFuel <- useFuel
      if hasFuel
        then do
          result <- tryInline fname args'
          restoreFuel -- Restore fuel: it acts purely as recursion depth limit
          case result of
            -- A symbolic address keeps the call as its residual expression:
            -- the value itself cannot be written in MAST.
            Just r | Just k <- resFreeOffset r -> pure (EvalResult residual (Just k))
            Just r -> pure r
            -- Not foldable to a value: the call stays, but it may still carry
            -- comptime-only arguments that have to be erased from the callee.
            Nothing ->
              plainResult . fromMaybe residual
                <$> tryCloneComptime i (map resExp args')
        else pure (plainResult residual)
evalExp env (MastCon i es) = do
  es' <- mapM (evalExp env) es
  pure $ plainResult (MastCon i (map resExp es'))
evalExp env (MastCond e1 e2 e3) = do
  -- Evaluate all branches (conservative approach)
  -- Could potentially simplify if condition is known literal
  e1' <- resExp <$> evalExp env e1
  e2' <- resExp <$> evalExp env e2
  e3' <- resExp <$> evalExp env e3
  pure $ plainResult (MastCond e1' e2' e3')

-----------------------------------------------------------------------
-- Primitive evaluation (named-function fast paths)
-----------------------------------------------------------------------

evalPrimitive :: Name -> [MastExp] -> Maybe MastExp
evalPrimitive (Name "subWord") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just (MastLit (IntLit (maskWord (a - b))))
evalPrimitive (Name "gtWord") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just $ mkBool (a > b)
evalPrimitive (Name "bxorWord") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just (MastLit (IntLit (maskWord (a `xor` b))))
evalPrimitive (Name "bandWord") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just (MastLit (IntLit (maskWord (a .&. b))))
evalPrimitive (Name "borWord") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just (MastLit (IntLit (maskWord (a .|. b))))
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
  Just (MastLit (IntLit (keccakInteger (TE.encodeUtf8 (T.pack s)))))
-- Integer (comptime-only, unlimited precision) primitives:
evalPrimitive (Name "wordToInteger") [MastLit (IntLit n)] =
  Just (MastLit (IntLit n)) -- value-level identity
evalPrimitive (Name "wordFromInteger") [MastLit (IntLit n)] =
  Just (MastLit (IntLit (maskWord n))) -- truncate to 256 bits
evalPrimitive (Name "integerAdd") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just (MastLit (IntLit (a + b))) -- exact, no overflow
evalPrimitive (Name "integerSub") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just (MastLit (IntLit (a - b)))
evalPrimitive (Name "integerMul") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just (MastLit (IntLit (a * b)))
evalPrimitive (Name "integerLt") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just (mkBool (a < b))
evalPrimitive (Name "integerEq") [MastLit (IntLit a), MastLit (IntLit b)] =
  Just (mkBool (a == b))
evalPrimitive (QualName (Name "Int") "fromInteger") [x] = Just x -- identity for integer -> integer
evalPrimitive (QualName (Name "Str") "fromString") [x] = Just x -- identity for string -> string
evalPrimitive _ _ = Nothing

-- | keccak256 of a byte sequence, as the 256-bit big-endian word EVM produces.
keccakInteger :: BS.ByteString -> Integer
keccakInteger bs = bsToIntegerBE (BA.convert digest)
  where
    digest :: Digest Keccak_256
    digest = hash bs

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

-- | The address at which the free memory pointer is kept.
freeMemPtrSlot :: Integer
freeMemPtrSlot = 64

-- | True if writing n bytes at this address would disturb the free memory
-- pointer.  The evaluator does not model reallocation — 'FreeOffset' assumes
-- the pointer stays put — so such a write aborts the evaluation.
touchesFreeMemPtr :: YulVal -> Integer -> Bool
touchesFreeMemPtr (Concrete p) n = p < freeMemPtrSlot + 32 && freeMemPtrSlot < p + n
touchesFreeMemPtr (FreeOffset _) _ = False

-- | Write a 256-bit value to memory at byte address p (big-endian, 32 bytes).
mstoreBytes :: MemAddr -> Integer -> Map.Map MemAddr Word8 -> Map.Map MemAddr Word8
mstoreBytes (r, p) v mem =
  foldl'
    (\m i -> Map.insert (r, p + i) (fromIntegral ((v `shiftR` (8 * (31 - fromIntegral i))) .&. 0xff)) m)
    mem
    [0 .. 31]

-- | Read a 256-bit value from memory at byte address p (big-endian, 32 bytes).
-- Returns Nothing if any byte in p..p+31 was not written during this comptime
-- evaluation. We cannot assume unwritten bytes are 0: runtime code may have
-- written to memory before this function executes (e.g. the free memory pointer
-- at slot 64 is set by initialization code before any user function runs).
mloadWord :: MemAddr -> Map.Map MemAddr Word8 -> Maybe Integer
mloadWord a mem = bsToIntegerBE <$> mloadRange a 32 mem

-- | Read n bytes from memory starting at byte address p.
-- Like 'mloadWord', returns Nothing unless every byte in the range was written
-- during this comptime evaluation.
mloadRange :: MemAddr -> Integer -> Map.Map MemAddr Word8 -> Maybe BS.ByteString
mloadRange (r, p) n mem = BS.pack <$> traverse (\i -> Map.lookup (r, p + i) mem) [0 .. n - 1]

-----------------------------------------------------------------------
-- Yul interpreter: expression and statement evaluator (in EvalM)
-----------------------------------------------------------------------

-- | Evaluate a Yul expression given the current Yul state.
-- Returns Nothing if any operand is unknown or the operation is unsupported.
-- Clause order matters: mload must precede the general YCall catch-all.
evalYulExp :: YulState -> YulExp -> EvalM (Maybe YulVal)
evalYulExp env (YIdent n) = pure (Map.lookup n env)
evalYulExp _ (YLit (YulNumber n)) = pure (Just (Concrete n))
evalYulExp _ (YLit YulTrue) = pure (Just (Concrete 1))
evalYulExp _ (YLit YulFalse) = pure (Just (Concrete 0))
evalYulExp env (YCall (Name "mload") [pExp]) = do
  compt <- askComptimeMode
  if not compt
    then pure Nothing -- mload is only meaningful in comptime context
    else do
      mp <- evalYulExp env pExp
      case mp of
        -- Reading the free memory pointer yields the symbolic base itself.
        Just (Concrete p) | p == freeMemPtrSlot -> pure (Just (FreeOffset 0))
        Just a -> withRegionOf a $ \addr mem -> Concrete <$> mloadWord addr mem
        Nothing -> pure Nothing
evalYulExp env (YCall (Name "keccak256") [pExp, nExp]) = do
  compt <- askComptimeMode
  if not compt
    then pure Nothing -- hashing memory is only meaningful in comptime context
    else do
      mp <- evalYulExp env pExp
      mn <- evalYulExp env nExp
      case (mp, mn) of
        -- A length is always a plain word; only the address may be symbolic.
        (Just a, Just (Concrete n)) ->
          withRegionOf a $ \addr mem -> Concrete . keccakInteger <$> mloadRange addr n mem
        _ -> pure Nothing
evalYulExp env (YCall op args) = do
  mvals <- mapM (evalYulExp env) args
  case sequence mvals of
    Nothing -> pure Nothing
    Just vals -> evalYulOp op vals
evalYulExp _ _ = pure Nothing

-- | Read comptime memory at the address denoted by a Yul value, provided that
-- address lies in the region this evaluation is already using.
withRegionOf :: YulVal -> (MemAddr -> Map.Map MemAddr Word8 -> Maybe a) -> EvalM (Maybe a)
withRegionOf a k = do
  let addr = memAddr a
  ok <- regionUsable (fst addr)
  if not ok
    then pure Nothing
    else do
      mem <- getsMem
      pure (k addr mem)

-- | Evaluate a Yul built-in operation on known values.
-- Returns Nothing for unsupported or unknown operations.
-- All operations use EVM semantics: unsigned 256-bit arithmetic, no exceptions.
-- Only add/sub accept the symbolic free-memory base, so it can be used as an
-- address but can never end up in a folded word.
evalYulOp :: Name -> [YulVal] -> EvalM (Maybe YulVal)
evalYulOp (Name "add") [FreeOffset k, Concrete n] = pure (Just (FreeOffset (k + n)))
evalYulOp (Name "add") [Concrete n, FreeOffset k] = pure (Just (FreeOffset (k + n)))
evalYulOp (Name "sub") [FreeOffset k, Concrete n] = pure (Just (FreeOffset (k - n)))
evalYulOp op args = pure $ do
  ns <- traverse concreteVal args
  Concrete <$> evalWordOp op ns

concreteVal :: YulVal -> Maybe Integer
concreteVal (Concrete n) = Just n
concreteVal (FreeOffset _) = Nothing

-- | The 256-bit word semantics of the supported Yul built-ins.
evalWordOp :: Name -> [Integer] -> Maybe Integer
evalWordOp (Name "add") [a, b] = Just (maskWord (a + b))
evalWordOp (Name "sub") [a, b] = Just (maskWord (a - b))
evalWordOp (Name "mul") [a, b] = Just (maskWord (a * b))
evalWordOp (Name "div") [a, b] = Just (if b == 0 then 0 else a `div` b)
evalWordOp (Name "mod") [a, b] = Just (if b == 0 then 0 else a `mod` b)
evalWordOp (Name "gt") [a, b] = Just (if a > b then 1 else 0)
evalWordOp (Name "lt") [a, b] = Just (if a < b then 1 else 0)
evalWordOp (Name "eq") [a, b] = Just (if a == b then 1 else 0)
evalWordOp (Name "iszero") [a] = Just (if a == 0 then 1 else 0)
evalWordOp (Name "and") [a, b] = Just (a .&. b)
evalWordOp (Name "or") [a, b] = Just (a .|. b)
evalWordOp (Name "xor") [a, b] = Just (a `xor` b)
evalWordOp (Name "not") [a] = Just (maskWord (complement a))
evalWordOp (Name "shl") [sh, v] = Just (maskWord (v `shiftL` fromIntegral sh))
evalWordOp (Name "shr") [sh, v] = Just (v `shiftR` fromIntegral sh)
evalWordOp _ _ = Nothing

-- | Evaluate one Yul statement, updating the Yul state.
-- mstore/mstore8 update EvalM memory; assignments update YulState.
-- Returns Nothing if the statement form is unsupported.
evalYulStmt :: YulState -> YulStmt -> EvalM (Maybe YulState)
evalYulStmt env (YAssign [n] e) = do
  mv <- evalYulExp env e
  pure (fmap (\v -> Map.insert n v env) mv)
evalYulStmt env (YExp (YCall (Name "mstore") [pExp, vExp])) =
  evalMemWrite env pExp vExp 32 $ \addr v -> mstoreBytes addr v
evalYulStmt env (YExp (YCall (Name "mstore8") [pExp, vExp])) =
  evalMemWrite env pExp vExp 1 $ \addr v -> Map.insert addr (fromIntegral (v .&. 0xff))
evalYulStmt _ _ = pure Nothing

-- | Common shape of mstore and mstore8: evaluate address and value, check that
-- the write is one the comptime memory model can represent, then apply it.
-- Returns Nothing (aborting the block) whenever it cannot, so that a function
-- whose body writes memory is never inlined on the strength of a skipped write.
evalMemWrite ::
  YulState ->
  YulExp ->
  YulExp ->
  Integer ->
  (MemAddr -> Integer -> Map.Map MemAddr Word8 -> Map.Map MemAddr Word8) ->
  EvalM (Maybe YulState)
evalMemWrite env pExp vExp width write = do
  compt <- askComptimeMode
  if not compt
    then pure Nothing -- memory writes only in comptime context
    else do
      mp <- evalYulExp env pExp
      mv <- evalYulExp env vExp
      case (mp, mv) of
        -- Moving the free memory pointer would invalidate the symbolic base.
        (Just a, _) | touchesFreeMemPtr a width -> pure Nothing
        -- Only concrete words are stored: a symbolic address written to memory
        -- would lose its base and could later be read back as a plain word.
        (Just a, Just (Concrete v)) -> do
          let addr = memAddr a
          ok <- regionUsable (fst addr)
          if not ok
            then pure Nothing
            else do
              modifyMem (write addr v)
              pure (Just env) -- YulState unchanged; memory updated in EvalM
        _ -> pure Nothing

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

-- | Extract known values from a VEnv into a YulState.
venvToYulState :: VEnv -> YulState
venvToYulState env = Map.fromList (concatMap entry (Map.toList env))
  where
    entry (k, EvalResult _ (Just off)) = [(mastIdName k, FreeOffset off)]
    entry (k, EvalResult (MastLit (IntLit v)) Nothing) = [(mastIdName k, Concrete v)]
    entry _ = []

-- | Build a name→YulExp substitution map from all known literal values in VEnv.
-- Used to inline comptime values into asm blocks so that eliminated 'let' bindings
-- remain available to the asm code.
venvToSubst :: VEnv -> Map.Map Name YulExp
venvToSubst env =
  Map.fromList
    [ (mastIdName k, yulLit l)
    | (k, EvalResult (MastLit l) Nothing) <- Map.toList env
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
        Nothing -> acc
        Just mastId -> case v of
          Concrete w -> Map.insert mastId (plainResult (MastLit (IntLit w))) acc
          -- A symbolic address is known to the evaluator but has no MAST form,
          -- so the variable itself stands for it in the emitted program.
          FreeOffset off -> Map.insert mastId (EvalResult (MastVar mastId) (Just off)) acc

-----------------------------------------------------------------------
-- Comptime-only parameter erasure
-----------------------------------------------------------------------

-- A `string` parameter has no runtime representation, so a function carrying
-- one can never be emitted.  Such a function survives specialisation only
-- because its argument is a comptime value: substituting that value into the
-- body and dropping the parameter is what makes it emittable.
--
-- This is not an optimisation but a requirement, and it is why the pass runs
-- here rather than in Specialise: by this point the argument has already been
-- folded, so `f(concatLit("ab", "cd"))` presents the same single literal to
-- the clone as `f("abcd")` does.
--
-- The motivating case is a source instance of the `Str` class, whose method
-- materializes its argument.  Inside the method body the argument is a
-- parameter, so the materializer would never see a literal; after cloning it
-- does, and EmitHull's per-literal intercept fires as usual.

-- Try to specialise a call by substituting literal arguments for the callee's
-- comptime-only parameters.  Returns a call to the clone, with those arguments
-- dropped.  Gives up (leaving the original call, and hence EmitHull's guard to
-- report it) when any such parameter has a non-literal argument.
tryCloneComptime :: MastId -> [MastExp] -> EvalM (Maybe MastExp)
tryCloneComptime i args = do
  mfd <- lookupFunDef (mastIdName i)
  case mfd of
    Nothing -> pure Nothing
    Just fd
      | length (mastFunParams fd) /= length args -> pure Nothing
      | null comptimeParams -> pure Nothing
      | otherwise -> case mapM literalOf comptimeParams of
          Nothing -> pure Nothing
          Just binds -> cloneWith fd binds args
      where
        comptimeParams =
          [ (p, a)
          | (p, a) <- zip (mastFunParams fd) args,
            mastParamType p == mastStringTy
          ]
        literalOf (p, MastLit l) = Just (p, l)
        literalOf _ = Nothing

-- Clone definitions are not in the (immutable) function table, so a clone
-- calling another cloneable function must be found in the state as well.
lookupFunDef :: Name -> EvalM (Maybe MastFunDef)
lookupFunDef n = do
  ft <- askFunTable
  clones <- lift $ gets esCloneDefs
  pure (Map.lookup n ft <|> Map.lookup n clones)

-- Create (or reuse) the clone of `fd` in which each listed parameter is bound
-- to its literal, and build the call to it with those arguments dropped.
--
-- The substitution itself is left to the evaluator: the clone is queued with a
-- variable environment binding the erased parameters, and evaluating its body
-- under that environment both substitutes the literals and folds whatever they
-- unlock.  This reuses the propagation `evalStmts` already performs rather
-- than adding a second, parallel substitution traversal.
cloneWith :: MastFunDef -> [(MastParam, Literal)] -> [MastExp] -> EvalM (Maybe MastExp)
cloneWith fd binds args = do
  known <- lift $ gets (Map.lookup key . esCloneNames)
  mname <- maybe register (pure . Just) known
  pure (call <$> mname)
  where
    call name' = MastCall (MastId name' cloneTy) keptArgs
    key = (mastFunName fd, [(mastParamName p, l) | (p, l) <- binds])
    boundNames = map (mastParamName . fst) binds
    isKept p = mastParamName p `notElem` boundNames
    keptParams = filter isKept (mastFunParams fd)
    keptArgs = [a | (p, a) <- zip (mastFunParams fd) args, isKept p]
    cloneTy = foldr (MastArrow . mastParamType) (mastFunReturn fd) keptParams
    env0 =
      Map.fromList
        [ (MastId (mastParamName p) (mastParamType p), plainResult (MastLit l))
        | (p, l) <- binds
        ]
    -- Each new clone costs fuel, permanently.  A recursive callee that derives
    -- a fresh literal on every step would otherwise queue clones forever;
    -- exhausting the budget stops that and is already reported to the user.
    register = do
      hasFuel <- useFuel
      if not hasFuel
        then pure Nothing
        else do
          n <- freshCloneName (mastFunName fd)
          let clone = fd {mastFunName = n, mastFunParams = keptParams}
          lift $
            modify
              ( \s ->
                  s
                    { esCloneNames = Map.insert key n (esCloneNames s),
                      esCloneDefs = Map.insert n clone (esCloneDefs s),
                      esPendingClones = (clone, env0) : esPendingClones s
                    }
              )
          pure (Just n)

freshCloneName :: Name -> EvalM Name
freshCloneName base = do
  n <- lift $ gets (Map.size . esCloneDefs)
  pure (suffixName base ("$ct" ++ show n))

suffixName :: Name -> String -> Name
suffixName (Name s) suffix = Name (s ++ suffix)
suffixName (QualName q s) suffix = QualName q (s ++ suffix)

-----------------------------------------------------------------------
-- Function inlining
-----------------------------------------------------------------------

-- Try to inline a function call.
-- Works when: (1) all arguments are known values, or
--             (2) function is "constant" (ignores its arguments)
-- Only pure functions (no non-interpretable asm, no impure calls) are eligible.
tryInline :: Name -> [EvalResult] -> EvalM (Maybe EvalResult)
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
                  -- '-> comptime' asserts the body is a compile-time computation,
                  -- so its memory operations may run even outside a comptime let.
                  inComptime = if mastFunRetComptime fd then withComptimeMode else id
              inComptime $ evalFunBody tyReg env (mastFunBody fd)

-- Evaluate a function body and extract the return value
evalFunBody :: TypeReg -> VEnv -> [MastStmt] -> EvalM (Maybe EvalResult)
evalFunBody _ _ [] = pure Nothing -- No return statement found
evalFunBody tyReg env (stmt : rest) = case stmt of
  MastLet _ i _ mInit -> do
    mInit' <- traverse (evalExp env) mInit
    let env' = case mInit' of
          Just r -> bindResult i r env
          Nothing -> Map.delete i env
    evalFunBody tyReg env' rest
  MastAssign i e -> do
    r <- evalExp env e
    evalFunBody tyReg (bindResult i r env) rest
  -- Evaluate for effect: a void call such as `mstore(p, v)` contributes
  -- nothing to the environment but does write comptime memory.
  MastStmtExp e -> do
    _ <- evalExp env e
    evalFunBody tyReg env rest
  MastReturn e -> do
    r <- evalExp env e
    pure $ if resIsKnown r then Just r else Nothing
  MastMatch scrut alts -> do
    scrut' <- resExp <$> evalExp env scrut
    alts' <- mapM (\(p, b) -> (,b) <$> evalPat env p) alts
    case matchAlts env scrut' alts' of
      Just (env', body) -> evalFunBody tyReg env' body
      Nothing -> pure Nothing -- Scrutinee not known, can't select branch
  MastFor {} -> pure Nothing -- Loop execution cannot be folded safely here
  MastBreak -> pure Nothing -- Control transfer cannot be folded
  MastContinue -> pure Nothing -- Control transfer cannot be folded
  MastAsm yul -> do
    -- Try to interpret the asm block statically.
    -- If successful, update env and continue inlining; otherwise give up.
    let yulState = venvToYulState env
    mYulState' <- evalYulBlock yulState yul
    case mYulState' of
      Just yulState' -> evalFunBody tyReg (mergeYulStateToVEnv tyReg yulState' env) rest
      Nothing -> pure Nothing
  MastSeq stmts -> evalFunBody tyReg env stmts

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
      let env' = Map.insert varId (plainResult (MastCon (MastId conName (mastIdType varId)) args)) env
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
      let env' = Map.insert varId (plainResult (MastLit lit)) env
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
              then Map.insert varId (plainResult arg) env
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

-- Primitives the PE evaluates directly; their std asm bodies are irrelevant.
-- Integer primitive names come from Primitives.integerPrimNames (single source
-- of truth shared with Specialise.comptimeBuiltins).
builtinPureFuns :: Set.Set Name
builtinPureFuns =
  Set.fromList $
    [ Name "subWord",
      Name "gtWord",
      Name "bxorWord",
      Name "bandWord",
      Name "borWord",
      Name "eqWord",
      Name "concatLit",
      Name "strlenLit",
      Name "keccakLit"
    ]
      ++ integerPrimNames
      ++ stringPrimNames

-- Functions with dummy pure bodies that are intercepted by EmitHull
builtinImpureFuns :: Set.Set Name
builtinImpureFuns = Set.fromList [Name "revertLit", memStringFromLitName]

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
stmtIsPure pureFuns (MastFor initStmt cond post body) =
  stmtIsPure pureFuns initStmt
    && expIsPure pureFuns cond
    && stmtIsPure pureFuns post
    && bodyIsPure pureFuns body
stmtIsPure _ MastBreak = True
stmtIsPure _ MastContinue = True
stmtIsPure pureFuns (MastSeq stmts) = bodyIsPure pureFuns stmts

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

    -- mload and keccak256 have their own clauses (they read memory);
    -- general YCall delegates to interpretableOp
    interpretableExp (YIdent _) = True
    interpretableExp (YLit (YulNumber _)) = True
    interpretableExp (YLit YulTrue) = True
    interpretableExp (YLit YulFalse) = True
    interpretableExp (YCall (Name "mload") [p]) = interpretableExp p
    interpretableExp (YCall (Name "keccak256") [p, n]) = interpretableExp p && interpretableExp n
    interpretableExp (YCall op args) =
      interpretableOp op (length args) && all interpretableExp args
    interpretableExp _ = False

    interpretableOp (Name "add") 2 = True
    interpretableOp (Name "sub") 2 = True
    interpretableOp (Name "mul") 2 = True
    interpretableOp (Name "div") 2 = True
    interpretableOp (Name "mod") 2 = True
    interpretableOp (Name "gt") 2 = True
    interpretableOp (Name "lt") 2 = True
    interpretableOp (Name "eq") 2 = True
    interpretableOp (Name "iszero") 1 = True
    interpretableOp (Name "and") 2 = True
    interpretableOp (Name "or") 2 = True
    interpretableOp (Name "xor") 2 = True
    interpretableOp (Name "not") 1 = True
    interpretableOp (Name "shl") 2 = True
    interpretableOp (Name "shr") 2 = True
    interpretableOp _ _ = False

-----------------------------------------------------------------------
-- Dead code elimination
-----------------------------------------------------------------------

-- | Remove unused functions from a compilation unit.
-- deployer  and 'main' are always considered roots (entry points).
eliminateDeadCode :: MastCompUnit -> MastCompUnit
eliminateDeadCode cu = cu {mastTopDecls = map elimTopDecl (mastTopDecls cu)}
  where
    elimTopDecl (MastTContr c) = MastTContr (elimContract c)
    elimTopDecl d@(MastTDataDef _) = d

    elimContract c = c {mastContrDecls = concatMap filterDecl (mastContrDecls c)}
      where
        usedNames = findUsedFunctions c
        -- Drop unreachable functions, recursing into mutual blocks so that
        -- dead members of a block whose siblings are still reachable are also
        -- removed (e.g. a helper the deployer only needed before partial
        -- evaluation folded its call). Data declarations are always kept.
        filterDecl d@(MastCFunDecl fd)
          | mastFunName fd `Set.member` usedNames = [d]
          | otherwise = []
        filterDecl (MastCMutualDecl ds) =
          case concatMap filterDecl ds of
            [] -> []
            ds' -> [MastCMutualDecl ds']
        filterDecl d@(MastCDataDecl _) = [d]

-- | Find all functions reachable from root functions
findUsedFunctions :: MastContract -> Set.Set Name
findUsedFunctions c = go initialRoots initialRoots
  where
    -- Root functions that are always considered used
    rootNames = Set.fromList [deployerName, Name "main"]

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
callsInStmt (MastFor initStmt cond post body) =
  Set.unions
    [ callsInStmt initStmt,
      callsInExp cond,
      callsInStmt post,
      Set.unions (map callsInStmt body)
    ]
callsInStmt (MastSeq stmts) = Set.unions (map callsInStmt stmts)
callsInStmt (MastAsm _) = Set.empty
callsInStmt MastBreak = Set.empty
callsInStmt MastContinue = Set.empty

callsInExp :: MastExp -> Set.Set Name
callsInExp (MastLit _) = Set.empty
callsInExp (MastVar _) = Set.empty
callsInExp (MastCall i args) =
  Set.insert (mastIdName i) (Set.unions (map callsInExp args))
callsInExp (MastCon _ args) = Set.unions (map callsInExp args)
callsInExp (MastCond e1 e2 e3) =
  Set.unions [callsInExp e1, callsInExp e2, callsInExp e3]
