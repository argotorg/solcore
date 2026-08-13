module Solcore.Backend.ComptimeCheck (checkComptime) where

{- MAST-level comptime verification pass.
   Runs on MastCompUnit after specialization and partial evaluation.

   Partial evaluation has already had its chance at every function here, so a
   comptime obligation counts as discharged only when nothing is left to
   compute: a literal, or a constructor applied to values.  A residual call is
   a runtime computation whatever its shape, which is what makes exhausted
   fuel a rejection rather than silently worse code.

   What the pass enforces:
     - A parameter annotated 'comptime' receives a value at every call site.
     - A 'let x : comptime T = e' binding has a value for e.
     - A function annotated '-> comptime T' returns a value.
     - No comptime annotation survives at all: every function reaching this
       pass is about to be emitted, so a comptime parameter means the argument
       was never substituted and a comptime return means a call to it was
       never folded.  Either way the promise the annotation makes is now
       unkeepable.

   The verifier reports the first violation found as a String error.
-}

import Data.Map qualified as Map
import Solcore.Backend.Mast
import Solcore.Backend.MastEval (FunTable, buildFunTable)
import Solcore.Frontend.Syntax.Name (Name)

-- | What the checker needs to know about the function it is walking.
data FunCtx = FunCtx
  { fcFunTable :: FunTable,
    fcRetComptime :: Bool,
    fcName :: Name
  }

-- | Entry point: check all functions in the compilation unit.
--
-- The residual-annotation check is a second pass rather than part of the first
-- so that the more specific diagnostics — naming the offending call site or
-- binding — are reported in preference to it.
checkComptime :: MastCompUnit -> Either String ()
checkComptime cu = do
  onEveryFunDef (checkFunDef ft) cu
  onEveryFunDef checkNothingLeftToSubstitute cu
  where
    ft = buildFunTable cu

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
checkFunDef :: FunTable -> MastFunDef -> Either String ()
checkFunDef ft fd = checkStmts fc (mastFunBody fd)
  where
    fc =
      FunCtx
        { fcFunTable = ft,
          fcRetComptime = mastFunRetComptime fd,
          fcName = mastFunName fd
        }

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

-- | Check a sequence of statements.
checkStmts :: FunCtx -> [MastStmt] -> Either String ()
checkStmts fc = mapM_ (checkStmt fc)

-- | Check one statement.
checkStmt :: FunCtx -> MastStmt -> Either String ()
checkStmt fc stmt = case stmt of
  MastLet ct i _ mInit ->
    case mInit of
      Nothing -> Right ()
      Just e -> do
        checkExp fc e
        when_ (ct && not (isValue e)) $
          "comptime let '" ++ show (mastIdName i) ++ "' is bound to a runtime expression"
  MastAssign _ e ->
    checkExp fc e
  MastStmtExp e ->
    checkExp fc e
  MastReturn e -> do
    checkExp fc e
    when_ (fcRetComptime fc && not (isValue e)) $
      "function '" ++ show (fcName fc) ++ "' annotated '-> comptime' returns a runtime expression"
  MastMatch scrut alts -> do
    checkExp fc scrut
    mapM_ (checkAlt fc) alts
  MastFor initStmt cond postStmt body -> do
    checkStmt fc initStmt
    checkExp fc cond
    checkStmt fc postStmt
    checkStmts fc body
  MastAsm _ ->
    Right ()
  MastBreak ->
    Right ()
  MastContinue ->
    Right ()
  MastSeq stmts ->
    checkStmts fc stmts

-- | Check an alternative in a match expression.
checkAlt :: FunCtx -> MastAlt -> Either String ()
checkAlt fc (_, body) = checkStmts fc body

-- | Check comptime-param constraints inside an expression (recursive).
checkExp :: FunCtx -> MastExp -> Either String ()
checkExp fc (MastCall f args) = do
  checkCallSite fc f args
  mapM_ (checkExp fc) args
checkExp fc (MastCon _ args) =
  mapM_ (checkExp fc) args
checkExp fc (MastCond c t e) =
  mapM_ (checkExp fc) [c, t, e]
checkExp _ _ = Right ()

-- | Verify that comptime-annotated parameters receive comptime arguments.
checkCallSite :: FunCtx -> MastId -> [MastExp] -> Either String ()
checkCallSite fc f args =
  case Map.lookup (mastIdName f) (fcFunTable fc) of
    Nothing -> Right () -- builtin or unknown; no annotation to check
    Just fd ->
      mapM_ checkArg (zip (mastFunParams fd) args)
  where
    checkArg (param, arg) =
      when_ (mastParamComptime param && not (isValue arg)) $
        "runtime value passed to comptime parameter '"
          ++ show (mastParamName param)
          ++ "' of '"
          ++ show (mastIdName f)
          ++ "'"

-- | A value: what a discharged comptime expression looks like once partial
-- evaluation has finished with it.  No computation left to perform.
isValue :: MastExp -> Bool
isValue (MastLit _) = True
isValue (MastCon _ args) = all isValue args
isValue _ = False

-- | Like 'when' but for Either.
when_ :: Bool -> String -> Either String ()
when_ True msg = Left msg
when_ False _ = Right ()
