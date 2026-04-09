module Solcore.Frontend.ComptimeCheck (checkComptimeEarly) where

{- SAIL-level comptime verification pass.
   Runs on CompUnit Id immediately after type checking.

   Classification uses three states:
     CTComptime  — definitely comptime: literal, comptime-bound variable, or
                   a call to a function annotated '-> comptime' with all
                   comptime-param arguments classified as CTComptime.
     CTRuntime   — definitely not comptime: a variable bound by a non-comptime
                   function parameter.
     CTDeferred  — uncertain: call results where the function has no comptime
                   return annotation, or unannotated let bindings.
                   These are passed to the MAST-level verifier.

   Errors are reported only for CTRuntime violations:
     - A parameter annotated 'comptime' receives a CTRuntime argument.
     - A 'let x : comptime T = e' binding where e classifies as CTRuntime.

   CTDeferred values are never rejected here; the MAST-level pass handles them.
-}

import Data.Map qualified as Map
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name (Name)
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty (Ty (..))
import Solcore.Frontend.TypeInference.Id (Id (..))

-----------------------------------------------------------------------
-- Comptime-ness classification
-----------------------------------------------------------------------

data Ctness = CTComptime | CTRuntime | CTDeferred
  deriving (Eq, Show)

-----------------------------------------------------------------------
-- Signature table: Name -> Signature Id
-----------------------------------------------------------------------

type SigTable = Map.Map Name (Signature Id)

buildSigTable :: CompUnit Id -> SigTable
buildSigTable (CompUnit _ topDecls) = Map.fromList $ concatMap fromTopDecl topDecls
  where
    fromTopDecl (TFunDef fd) = [(sigName (funSignature fd), funSignature fd)]
    fromTopDecl (TContr c) = concatMap fromContrDecl (decls c)
    fromTopDecl (TClassDef cl) = [(sigName s, s) | s <- signatures cl]
    fromTopDecl (TInstDef inst) = [(sigName (funSignature fd), funSignature fd) | fd <- instFunctions inst]
    fromTopDecl _ = []

    fromContrDecl (CFunDecl fd) = [(sigName (funSignature fd), funSignature fd)]
    fromContrDecl _ = []

-----------------------------------------------------------------------
-- Comptime environment: variable name -> Ctness
-----------------------------------------------------------------------

type CtEnv = Map.Map Name Ctness

-----------------------------------------------------------------------
-- Entry point
-----------------------------------------------------------------------

-- | Run the early comptime check on a typed compilation unit.
checkComptimeEarly :: CompUnit Id -> Either String ()
checkComptimeEarly cu = mapM_ (checkTopDecl st) (contracts cu)
  where
    st = buildSigTable cu

checkTopDecl :: SigTable -> TopDecl Id -> Either String ()
checkTopDecl st (TFunDef fd) = checkFunDef st ctx fd
  where
    ctx = "function '" ++ show (sigName (funSignature fd)) ++ "'"
checkTopDecl st (TContr c) = mapM_ (checkContrDecl st) (decls c)
checkTopDecl st (TInstDef inst) = mapM_ (checkFunDefInst st inst) (instFunctions inst)
checkTopDecl _ _ = Right ()

checkContrDecl :: SigTable -> ContractDecl Id -> Either String ()
checkContrDecl st (CFunDecl fd) = checkFunDef st ctx fd
  where
    ctx = "function '" ++ show (sigName (funSignature fd)) ++ "'"
checkContrDecl _ _ = Right ()

-----------------------------------------------------------------------
-- Function checking
-----------------------------------------------------------------------

checkFunDef :: SigTable -> String -> FunDef Id -> Either String ()
checkFunDef st ctx fd = checkBody st (sigRetComptime sig) ctx initEnv (funDefBody fd)
  where
    sig = funSignature fd
    initEnv =
      Map.fromList
        [ (idName (paramName p), if paramComptime p then CTComptime else CTRuntime)
          | p <- sigParams sig
        ]

-- | Check an instance method, including the instance head in error context.
checkFunDefInst :: SigTable -> Instance Id -> FunDef Id -> Either String ()
checkFunDefInst st inst fd = checkFunDef st ctx fd
  where
    ctx =
      "in instance "
        ++ tyHeadName (mainTy inst)
        ++ ":"
        ++ show (instName inst)
        ++ ", function '"
        ++ show (sigName (funSignature fd))
        ++ "'"

-- | Extract a readable name from a concrete type (e.g. @word@ from @TyCon "word" []@).
tyHeadName :: Ty -> String
tyHeadName (TyCon n _) = show n
tyHeadName t = show t

checkBody :: SigTable -> Bool -> String -> CtEnv -> Body Id -> Either String ()
checkBody _ _ _ _ [] = Right ()
checkBody st retCt ctx env (s : ss) = do
  env' <- checkStmt st retCt ctx env s
  checkBody st retCt ctx env' ss

checkStmt :: SigTable -> Bool -> String -> CtEnv -> Stmt Id -> Either String CtEnv
checkStmt st retCt ctx env stmt = case stmt of
  Let ct x _ mInit -> do
    case mInit of
      Nothing -> return env
      Just e -> do
        checkExp st env e
        let ct' = classifyExp st env e
        when_ (ct && ct' == CTRuntime) $
          "comptime let '"
            ++ show (idName x)
            ++ "' is bound to a runtime expression"
        return $ Map.insert (idName x) (letCtness ct ct') env
  (_ := e) -> checkExp st env e >> return env
  StmtExp e -> checkExp st env e >> return env
  Return e -> do
    checkExp st env e
    when_ (retCt && classifyExp st env e == CTRuntime) $
      ctx ++ ": function annotated '-> comptime' returns a runtime expression"
    return env
  Match es eqs -> do
    mapM_ (checkExp st env) es
    mapM_ (checkEq st retCt ctx env) eqs
    return env
  If cond t f -> do
    checkExp st env cond
    checkBody st retCt ctx env t
    checkBody st retCt ctx env f
    return env
  Asm _ -> return env

-- | Decide the Ctness to assign to a let-bound variable.
--   If declared comptime, treat as CTComptime (Stage 1 verifies the RHS).
--   Otherwise, inherit the classification of the init expression.
letCtness :: Bool -> Ctness -> Ctness
letCtness True _ = CTComptime
letCtness False ct' = ct'

checkEq :: SigTable -> Bool -> String -> CtEnv -> ([Pat Id], Body Id) -> Either String ()
checkEq st retCt ctx env (_, body) = checkBody st retCt ctx env body

-----------------------------------------------------------------------
-- Expression checking: recurse and enforce comptime-param constraints
-----------------------------------------------------------------------

checkExp :: SigTable -> CtEnv -> Exp Id -> Either String ()
checkExp st env (Call _ f args) = do
  checkCallSite st env f args
  mapM_ (checkExp st env) args
checkExp st env (Con _ args) = mapM_ (checkExp st env) args
checkExp st env (Cond c t e) = mapM_ (checkExp st env) [c, t, e]
checkExp st env (TyExp e _) = checkExp st env e
checkExp st env (Lam ps body _) = checkBody st False "lambda" lamEnv body
  where
    lamEnv =
      Map.fromList
        [(idName (paramName p), if paramComptime p then CTComptime else CTRuntime) | p <- ps]
        `Map.union` env
checkExp _ _ _ = Right ()

-- | Verify that each comptime-annotated parameter receives a non-Runtime arg.
--   Skips polymorphic signatures (those whose comptime parameter types contain
--   type variables): the concrete types are only known after specialisation,
--   so polymorphic calls are deferred to the MAST-level check.
checkCallSite :: SigTable -> CtEnv -> Id -> [Exp Id] -> Either String ()
checkCallSite st env f args =
  case Map.lookup (idName f) st of
    Nothing -> Right ()
    Just sig
      | any (hasTypeVar . paramTy) (filter paramComptime (sigParams sig)) ->
          Right () -- polymorphic comptime param — defer to MAST-level check
      | otherwise ->
          mapM_ checkArg (zip (sigParams sig) args)
  where
    checkArg (param, arg) =
      when_ (paramComptime param && classifyExp st env arg == CTRuntime) $
        "runtime value passed to comptime parameter '"
          ++ show (idName (paramName param))
          ++ "' of '"
          ++ show (idName f)
          ++ "'"
    paramTy (Typed _ _ ty) = ty
    paramTy (Untyped _ _) = TyCon (error "paramTy: Untyped") []

-- | True if the type contains any type variable or meta variable.
hasTypeVar :: Ty -> Bool
hasTypeVar (TyVar _) = True
hasTypeVar (Meta _) = True
hasTypeVar (TyCon _ ts) = any hasTypeVar ts

-----------------------------------------------------------------------
-- Expression classification
-----------------------------------------------------------------------

classifyExp :: SigTable -> CtEnv -> Exp Id -> Ctness
classifyExp _ _ (Lit _) = CTComptime
classifyExp _ env (Var x) = Map.findWithDefault CTDeferred (idName x) env
classifyExp st env (TyExp e _) = classifyExp st env e
classifyExp st env (Call _ f args) = classifyCall st env f args
classifyExp st env (Con _ args) = combineCt (map (classifyExp st env) args)
classifyExp st env (Cond c t e) = combineCt (map (classifyExp st env) [c, t, e])
classifyExp _ _ _ = CTDeferred

-- | Combine a list of Ctness values: all Comptime → Comptime;
--   any Runtime → Runtime; otherwise Deferred.
combineCt :: [Ctness] -> Ctness
combineCt cts
  | all (== CTComptime) cts = CTComptime
  | any (== CTRuntime) cts = CTRuntime
  | otherwise = CTDeferred

-- | Classify a function call result.
--   CTComptime iff the function is annotated '-> comptime' and every
--   argument to a comptime-annotated parameter is CTComptime.
--   Never CTRuntime for calls — uncertain cases are deferred to Stage 1.
classifyCall :: SigTable -> CtEnv -> Id -> [Exp Id] -> Ctness
classifyCall st env f args =
  case Map.lookup (idName f) st of
    Nothing -> CTDeferred
    Just sig
      | sigRetComptime sig && allCtParamsAreComptime sig ->
          CTComptime
      | otherwise ->
          CTDeferred
  where
    allCtParamsAreComptime sig =
      all
        (\(param, arg) -> not (paramComptime param) || classifyExp st env arg == CTComptime)
        (zip (sigParams sig) args)

-----------------------------------------------------------------------
-- Helper
-----------------------------------------------------------------------

when_ :: Bool -> String -> Either String ()
when_ True msg = Left msg
when_ False _ = Right ()
