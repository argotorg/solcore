module Solcore.Backend.ComptimeCheck (checkComptime) where

{- MAST-level comptime verification pass.
   Runs on MastCompUnit after specialization and partial evaluation.

   Two independent concerns:
     1. Classification: is an expression comptime?
        This pass runs after partial evaluation, so the question it asks
        depends on what evaluation still had left to do.

        In a closed context — a function with no comptime parameters and no
        comptime return — every comptime argument the body could receive has
        already been substituted, so partial evaluation has had its chance:
        only a value (a literal, or a constructor applied to values) counts.
        A residual call is a runtime computation whatever its shape, which is
        what makes exhausted fuel a rejection rather than silently worse code.

        In an open context the comptime parameters are still parameters, so
        the most that can be asked is the structural classification: a
        literal, a comptime-bound variable, or a call to a pure function whose
        arguments are all comptime.  Purity is determined by
        computeComptimePureFuns (MastEval), the stricter notion that excludes
        memory-op (mload/mstore) functions: this classifier has none of the
        runtime gating that makes memory folding sound in the evaluator.

     2. Constraint checking: annotations must be consistent with reality.
        - A parameter annotated 'comptime' must receive a comptime argument
          at every call site.
        - A 'let x : comptime T = e' binding requires e to be comptime.
        - A function annotated '-> comptime T' requires every returned
          expression to be comptime.

   The verifier reports the first violation found as a String error.
-}

import Data.Map qualified as Map
import Data.Set qualified as Set
import Solcore.Backend.Mast
import Solcore.Backend.MastEval (FunTable, buildFunTable, computeComptimePureFuns)
import Solcore.Frontend.Syntax.Name (Name)

-- | Set of variable names known to be comptime in the current scope.
type ComptimeEnv = Set.Set Name

-- | What the checker needs to know about the function it is walking.
data FunCtx = FunCtx
  { fcFunTable :: FunTable,
    fcPure :: Set.Set Name,
    fcRetComptime :: Bool,
    fcName :: Name,
    -- | True when comptime values reach this body as parameters rather than
    -- as substituted values, so a comptime expression need not be a value yet.
    fcOpen :: Bool
  }

-- | Entry point: check all functions in the compilation unit.
--
-- The residual-annotation check is a second pass rather than part of the first
-- so that the more specific diagnostics — naming the offending call site or
-- binding — are reported in preference to it.
checkComptime :: MastCompUnit -> Either String ()
checkComptime cu = do
  onEveryFunDef (checkFunDef ft pure_) cu
  onEveryFunDef checkNothingLeftToSubstitute cu
  where
    ft = buildFunTable cu
    pure_ = computeComptimePureFuns ft

-- | Apply a check to every function definition in the unit.
onEveryFunDef :: (MastFunDef -> Either String ()) -> MastCompUnit -> Either String ()
onEveryFunDef f cu = mapM_ inTopDecl (mastTopDecls cu)
  where
    inTopDecl (MastTContr c) = mapM_ inDecl (mastContrDecls c)
    inTopDecl (MastTDataDef _) = Right ()
    inDecl (MastCFunDecl fd) = f fd
    inDecl (MastCMutualDecl ds) = mapM_ inDecl ds
    inDecl (MastCDataDecl _) = Right ()

-- | Check a single function definition.
checkFunDef :: FunTable -> Set.Set Name -> MastFunDef -> Either String ()
checkFunDef ft pure_ fd = checkStmts fc initEnv (mastFunBody fd)
  where
    fc =
      FunCtx
        { fcFunTable = ft,
          fcPure = pure_,
          fcRetComptime = mastFunRetComptime fd,
          fcName = mastFunName fd,
          fcOpen =
            any mastParamComptime (mastFunParams fd)
              || mastFunRetComptime fd
        }
    -- For '-> comptime' functions, assume ALL params are comptime when checking
    -- the body: this verifies "if all args happen to be comptime, is the result?"
    -- For other functions, only explicitly-annotated comptime params are trusted.
    initEnv =
      Set.fromList
        [ mastParamName p
        | p <- mastFunParams fd,
          mastParamComptime p || mastFunRetComptime fd
        ]

-- | Every function reaching this pass is about to be emitted, so no comptime
-- annotation on it can still be honoured: a comptime parameter means the
-- argument was never substituted, and a comptime return means a call to it was
-- never folded. Either way the promise the annotation makes is now unkeepable.
checkNothingLeftToSubstitute :: MastFunDef -> Either String ()
checkNothingLeftToSubstitute fd = do
  case filter mastParamComptime (mastFunParams fd) of
    [] -> Right ()
    (p : _) ->
      Left $
        "comptime parameter '"
          ++ show (mastParamName p)
          ++ "' of '"
          ++ show (mastFunName fd)
          ++ "' survived partial evaluation"
  when_ (mastFunRetComptime fd) $
    "function '"
      ++ show (mastFunName fd)
      ++ "' annotated '-> comptime' survived partial evaluation"

-- | Check a sequence of statements, threading the comptime environment.
checkStmts :: FunCtx -> ComptimeEnv -> [MastStmt] -> Either String ()
checkStmts _ _ [] = Right ()
checkStmts fc env (s : ss) = do
  env' <- checkStmt fc env s
  checkStmts fc env' ss

-- | Check one statement; returns the updated comptime environment.
checkStmt :: FunCtx -> ComptimeEnv -> MastStmt -> Either String ComptimeEnv
checkStmt fc env stmt = case stmt of
  MastLet ct i _ mInit -> do
    case mInit of
      Nothing -> return env
      Just e -> do
        checkExp fc env e
        let ct' = discharged fc env e
        when_ (ct && not ct') $
          "comptime let '" ++ show (mastIdName i) ++ "' is bound to a runtime expression"
        return $ if ct || ct' then Set.insert (mastIdName i) env else env
  MastAssign i e -> do
    checkExp fc env e
    -- Whatever the variable held before, it now holds the result of this
    -- assignment, so it is no longer a known comptime value.
    return $ Set.delete (mastIdName i) env
  MastStmtExp e -> do
    checkExp fc env e
    return env
  MastReturn e -> do
    checkExp fc env e
    when_ (fcRetComptime fc && not (discharged fc env e)) $
      "function '" ++ show (fcName fc) ++ "' annotated '-> comptime' returns a runtime expression"
    return env
  MastMatch scrut alts -> do
    checkExp fc env scrut
    mapM_ (checkAlt fc env) alts
    return env
  MastFor initStmt cond postStmt body -> do
    _ <- checkStmt fc env initStmt
    checkExp fc env cond
    _ <- checkStmt fc env postStmt
    mapM_ (checkStmt fc env) body
    return env
  MastAsm _ ->
    return env
  MastBreak ->
    return env
  MastContinue ->
    return env
  MastSeq stmts -> do
    checkStmts fc env stmts
    return env

-- | Check an alternative in a match expression.
checkAlt :: FunCtx -> ComptimeEnv -> MastAlt -> Either String ()
checkAlt fc env (_, body) = checkStmts fc env body

-- | Check comptime-param constraints inside an expression (recursive).
checkExp :: FunCtx -> ComptimeEnv -> MastExp -> Either String ()
checkExp fc env (MastCall f args) = do
  checkCallSite fc env f args
  mapM_ (checkExp fc env) args
checkExp fc env (MastCon _ args) =
  mapM_ (checkExp fc env) args
checkExp fc env (MastCond c t e) =
  mapM_ (checkExp fc env) [c, t, e]
checkExp _ _ _ = Right ()

-- | Verify that comptime-annotated parameters receive comptime arguments.
checkCallSite :: FunCtx -> ComptimeEnv -> MastId -> [MastExp] -> Either String ()
checkCallSite fc env f args =
  case Map.lookup (mastIdName f) (fcFunTable fc) of
    Nothing -> Right () -- builtin or unknown; no annotation to check
    Just fd ->
      mapM_ checkArg (zip (mastFunParams fd) args)
  where
    checkArg (param, arg) =
      when_ (mastParamComptime param && not (discharged fc env arg)) $
        "runtime value passed to comptime parameter '"
          ++ show (mastParamName param)
          ++ "' of '"
          ++ show (mastIdName f)
          ++ "'"

-- | Has this expression's comptime obligation actually been met?
discharged :: FunCtx -> ComptimeEnv -> MastExp -> Bool
discharged fc env e
  | fcOpen fc = isComptime (fcFunTable fc) (fcPure fc) env e
  | otherwise = isValue e

-- | A value: what a discharged comptime expression looks like once partial
-- evaluation has finished with it.  No computation left to perform.
isValue :: MastExp -> Bool
isValue (MastLit _) = True
isValue (MastCon _ args) = all isValue args
isValue _ = False

-- | Classify an expression as comptime (True) or runtime (False).
--
-- A value is comptime if it is:
--   - a literal
--   - a variable bound in the comptime environment
--   - a call to a pure function with all comptime arguments
--   - a constructor applied to all comptime arguments
--   - a conditional whose scrutinee and both branches are comptime
isComptime :: FunTable -> Set.Set Name -> ComptimeEnv -> MastExp -> Bool
isComptime _ _ _ (MastLit _) = True
isComptime _ _ env (MastVar i) = mastIdName i `Set.member` env
isComptime ft pure_ env (MastCall f args) =
  mastIdName f `Set.member` pure_ && all (isComptime ft pure_ env) args
isComptime ft pure_ env (MastCon _ args) =
  all (isComptime ft pure_ env) args
isComptime ft pure_ env (MastCond c t e) =
  isComptime ft pure_ env c
    && isComptime ft pure_ env t
    && isComptime ft pure_ env e

-- | Like 'when' but for Either.
when_ :: Bool -> String -> Either String ()
when_ True msg = Left msg
when_ False _ = Right ()
