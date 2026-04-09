module Solcore.Backend.ComptimeCheck (checkComptime) where

{- MAST-level comptime verification pass.
   Runs on MastCompUnit after specialization and partial evaluation.

   Two independent concerns:
     1. Classification: is an expression statically comptime?
        A value is comptime if it is a literal, a comptime-bound variable,
        or a call to a pure function whose arguments are all comptime.
        Purity is determined by computePureFuns (MastEval).

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
import Solcore.Backend.MastEval (FunTable, buildFunTable, computePureFuns)
import Solcore.Frontend.Syntax.Name (Name)

-- | Set of variable names known to be comptime in the current scope.
type ComptimeEnv = Set.Set Name

-- | Entry point: check all functions in the compilation unit.
checkComptime :: MastCompUnit -> Either String ()
checkComptime cu = mapM_ checkTopDecl (mastTopDecls cu)
  where
    ft = buildFunTable cu
    pure_ = computePureFuns ft

    checkTopDecl (MastTContr c) = mapM_ (checkContractDecl ft pure_) (mastContrDecls c)
    checkTopDecl (MastTDataDef _) = Right ()

checkContractDecl :: FunTable -> Set.Set Name -> MastContractDecl -> Either String ()
checkContractDecl ft pure_ (MastCFunDecl fd) = checkFunDef ft pure_ fd
checkContractDecl ft pure_ (MastCMutualDecl ds) = mapM_ (checkContractDecl ft pure_) ds
checkContractDecl _ _ (MastCDataDecl _) = Right ()

-- | Check a single function definition.
checkFunDef :: FunTable -> Set.Set Name -> MastFunDef -> Either String ()
checkFunDef ft pure_ fd =
  checkStmts ft pure_ (mastFunRetComptime fd) (mastFunName fd) initEnv (mastFunBody fd)
  where
    initEnv = Set.fromList [mastParamName p | p <- mastFunParams fd, mastParamComptime p]

-- | Check a sequence of statements, threading the comptime environment.
checkStmts :: FunTable -> Set.Set Name -> Bool -> Name -> ComptimeEnv -> [MastStmt] -> Either String ()
checkStmts _ _ _ _ _ [] = Right ()
checkStmts ft pure_ retCt fname env (s : ss) = do
  env' <- checkStmt ft pure_ retCt fname env s
  checkStmts ft pure_ retCt fname env' ss

-- | Check one statement; returns the updated comptime environment.
checkStmt :: FunTable -> Set.Set Name -> Bool -> Name -> ComptimeEnv -> MastStmt -> Either String ComptimeEnv
checkStmt ft pure_ retCt fname env stmt = case stmt of
  MastLet ct i _ mInit -> do
    case mInit of
      Nothing -> return env
      Just e -> do
        checkExp ft pure_ env e
        let ct' = isComptime ft pure_ env e
        when_ (ct && not ct') $
          "comptime let '" ++ show (mastIdName i) ++ "' is bound to a runtime expression"
        return $ if ct || ct' then Set.insert (mastIdName i) env else env
  MastAssign _ e -> do
    checkExp ft pure_ env e
    return env
  MastStmtExp e -> do
    checkExp ft pure_ env e
    return env
  MastReturn e -> do
    checkExp ft pure_ env e
    let ct' = isComptime ft pure_ env e
    when_ (retCt && not ct') $
      "function '" ++ show fname ++ "' annotated '-> comptime' returns a runtime expression"
    return env
  MastMatch scrut alts -> do
    checkExp ft pure_ env scrut
    mapM_ (checkAlt ft pure_ retCt fname env) alts
    return env
  MastAsm _ ->
    return env

-- | Check an alternative in a match expression.
checkAlt :: FunTable -> Set.Set Name -> Bool -> Name -> ComptimeEnv -> MastAlt -> Either String ()
checkAlt ft pure_ retCt fname env (_, body) =
  checkStmts ft pure_ retCt fname env body

-- | Check comptime-param constraints inside an expression (recursive).
checkExp :: FunTable -> Set.Set Name -> ComptimeEnv -> MastExp -> Either String ()
checkExp ft pure_ env (MastCall f args) = do
  checkCallSite ft pure_ env f args
  mapM_ (checkExp ft pure_ env) args
checkExp ft pure_ env (MastCon _ args) =
  mapM_ (checkExp ft pure_ env) args
checkExp ft pure_ env (MastCond c t e) =
  mapM_ (checkExp ft pure_ env) [c, t, e]
checkExp _ _ _ _ = Right ()

-- | Verify that comptime-annotated parameters receive comptime arguments.
checkCallSite :: FunTable -> Set.Set Name -> ComptimeEnv -> MastId -> [MastExp] -> Either String ()
checkCallSite ft pure_ env f args =
  case Map.lookup (mastIdName f) ft of
    Nothing -> Right () -- builtin or unknown; no annotation to check
    Just fd ->
      mapM_ checkArg (zip (mastFunParams fd) args)
  where
    checkArg (param, arg) =
      when_ (mastParamComptime param && not (isComptime ft pure_ env arg)) $
        "runtime value passed to comptime parameter '"
          ++ show (mastParamName param)
          ++ "' of '"
          ++ show (mastIdName f)
          ++ "'"

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
