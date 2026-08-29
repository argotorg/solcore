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

   Comptime bindings are also immutable.  Partial evaluation discharges such a
   binding and erases its declaration, so an assignment to it would be left
   referring to a variable that no longer exists.  Assignment is therefore
   rejected here, where there is still a source span to point at, whatever the
   right-hand side is.
-}

import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.Yul
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name (Name)
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.TypeInference.Id (Id (..))
import Solcore.Primitives.Primitives qualified as Prim

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
-- Comptime environment: variable name -> what is known about it
-----------------------------------------------------------------------

data CtInfo = CtInfo
  { ctness :: Ctness,
    -- | True when the binding was written 'comptime' — a comptime let or a
    -- comptime parameter.  A binding merely /classified/ comptime is not
    -- declared comptime, and stays assignable.
    ctDeclared :: Bool
  }

type CtEnv = Map.Map Name CtInfo

-- | What the checker needs to know about the function it is walking.
data FunCtx = FunCtx
  { fcSigs :: SigTable,
    fcRetComptime :: Bool,
    -- | Human-readable description of the enclosing definition, for errors.
    fcWhere :: String,
    -- | Names assigned anywhere in the body.  What such a variable holds later
    -- is not what it was initialised with, so its initialiser cannot make it
    -- comptime.
    fcAssigned :: Set.Set Name
  }

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
checkFunDef st ctx fd = checkBody fc initEnv body
  where
    sig = funSignature fd
    body = funDefBody fd
    fc =
      FunCtx
        { fcSigs = st,
          fcRetComptime = effRetComptime sig,
          fcWhere = ctx,
          fcAssigned = assignedNames body
        }
    -- For '-> comptime' functions, treat ALL params as CTComptime when checking
    -- the body: this verifies "given comptime args, does the body produce comptime?"
    -- A param of comptime-only type (string/integer) is also implicitly comptime,
    -- since such values exist only at compile time.  Other params are CTRuntime.
    initEnv =
      Map.fromList
        [ (idName (paramName p), CtInfo (paramCtness p) (paramComptime p))
        | p <- sigParams sig
        ]
    paramCtness p
      | paramComptime p || effRetComptime sig || isComptimeOnlyTy (paramTy p) = CTComptime
      | otherwise = CTRuntime

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

checkBody :: FunCtx -> CtEnv -> Body Id -> Either String ()
checkBody _ _ [] = Right ()
checkBody fc env (s : ss) = do
  env' <- checkStmt fc env s
  checkBody fc env' ss

checkStmt :: FunCtx -> CtEnv -> Stmt Id -> Either String CtEnv
checkStmt fc env stmt = case stmt of
  Let ct x _ mInit -> do
    case mInit of
      Nothing ->
        -- Recorded even without an initialiser, so that assigning to it is
        -- still recognised as assigning to a comptime binding.
        return $ bind x (CtInfo (if ct then CTComptime else CTDeferred) ct) env
      Just e -> do
        checkExp fc env e
        let ct' = classifyExp fc env e
        when_ (ct && ct' == CTRuntime) $
          "comptime let '"
            ++ show (idName x)
            ++ "' is bound to a runtime expression"
        return $ bind x (CtInfo (letCtness fc ct ct' (idName x)) ct) env
  (lhs := rhs) -> do
    checkExp fc env lhs
    checkExp fc env rhs
    mapM_ (rejectComptimeTarget env) (assignTarget lhs)
    return env
  StmtExp e -> checkExp fc env e >> return env
  Return e -> do
    checkExp fc env e
    when_ (fcRetComptime fc && classifyExp fc env e == CTRuntime) $
      fcWhere fc ++ ": function annotated '-> comptime' returns a runtime expression"
    return env
  Match es eqs -> do
    mapM_ (checkExp fc env) es
    mapM_ (checkEq fc env) eqs
    return env
  If cond t f -> do
    checkExp fc env cond
    checkBody fc env t
    checkBody fc env f
    return env
  For initStmt _ postStmt body -> do
    _ <- checkStmt fc env initStmt
    checkBody fc env body
    _ <- checkStmt fc env postStmt
    return env
  Asm blk -> do
    -- A Yul block assigns to enclosing variables by name, so it can violate
    -- comptime immutability exactly as a SAIL assignment can.
    mapM_ (rejectComptimeTarget env) (Set.toList (yulAssignedNames blk))
    return env
  Block body -> checkBody fc env body >> return env
  Break -> return env
  Continue -> return env
  EmptyStmt -> return env
  where
    bind x = Map.insert (idName x)

-- | Reject an assignment whose target was declared comptime.
rejectComptimeTarget :: CtEnv -> Name -> Either String ()
rejectComptimeTarget env n =
  when_ (maybe False ctDeclared (Map.lookup n env)) $
    "cannot assign to comptime binding '" ++ show n ++ "'"

-- | The variable an assignment writes to, if it writes to one directly.
--   Assignments through a field or index have no single target name.
assignTarget :: Exp Id -> [Name]
assignTarget (Var x) = [idName x]
assignTarget (TyExp e _) = assignTarget e
assignTarget _ = []

-- | Decide the Ctness to assign to a let-bound variable.
--   If declared comptime, treat as CTComptime (the RHS check verifies it).
--   Otherwise inherit the classification of the init expression — unless the
--   variable is assigned later, in which case defer to the MAST-level check.
letCtness :: FunCtx -> Bool -> Ctness -> Name -> Ctness
letCtness _ True _ _ = CTComptime
letCtness fc False ct' n
  | n `Set.member` fcAssigned fc = CTDeferred
  | otherwise = ct'

checkEq :: FunCtx -> CtEnv -> ([Pat Id], Body Id) -> Either String ()
checkEq fc env (_, body) = checkBody fc env body

-----------------------------------------------------------------------
-- Assigned variables
-----------------------------------------------------------------------

-- | Names assigned anywhere in a body, by a SAIL assignment or inside a Yul
--   block.  Over-approximating is safe: it only defers a classification.
assignedNames :: Body Id -> Set.Set Name
assignedNames = foldMap inStmt
  where
    inStmt stmt = case stmt of
      (lhs := _) -> Set.fromList (assignTarget lhs)
      Asm blk -> yulAssignedNames blk
      Match _ eqs -> foldMap (assignedNames . snd) eqs
      If _ t f -> assignedNames t <> assignedNames f
      For initStmt _ postStmt body ->
        inStmt initStmt <> inStmt postStmt <> assignedNames body
      Block body -> assignedNames body
      Let {} -> Set.empty
      StmtExp _ -> Set.empty
      Return _ -> Set.empty
      Break -> Set.empty
      Continue -> Set.empty
      EmptyStmt -> Set.empty

-- | Names a Yul block assigns to, excluding the ones it declares itself.
yulAssignedNames :: YulBlock -> Set.Set Name
yulAssignedNames blk = assigned blk `Set.difference` declared blk
  where
    assigned = foldMap stmtAssigned
    stmtAssigned stmt = case stmt of
      YAssign ns _ -> Set.fromList ns
      YBlock b -> assigned b
      YFun _ _ _ ss -> assigned ss
      YIf _ b -> assigned b
      YSwitch _ cases dflt -> foldMap (assigned . snd) cases <> foldMap assigned dflt
      YFor pre _ post body -> assigned pre <> assigned post <> assigned body
      _ -> Set.empty

    declared = foldMap stmtDeclared
    stmtDeclared stmt = case stmt of
      YLet ns _ -> Set.fromList ns
      YBlock b -> declared b
      YFun _ args _ ss -> Set.fromList args <> declared ss
      YIf _ b -> declared b
      YSwitch _ cases dflt -> foldMap (declared . snd) cases <> foldMap declared dflt
      YFor pre _ post body -> declared pre <> declared post <> declared body
      _ -> Set.empty

-----------------------------------------------------------------------
-- Expression checking: recurse and enforce comptime-param constraints
-----------------------------------------------------------------------

checkExp :: FunCtx -> CtEnv -> Exp Id -> Either String ()
checkExp fc env (Call _ f args) = do
  checkCallSite fc env f args
  mapM_ (checkExp fc env) args
checkExp fc env (Con _ args) = mapM_ (checkExp fc env) args
checkExp fc env (Cond c t e) = mapM_ (checkExp fc env) [c, t, e]
checkExp fc env (TyExp e _) = checkExp fc env e
checkExp fc env (Lam ps body _) = checkBody lamCtx lamEnv body
  where
    lamCtx =
      fc
        { fcRetComptime = False,
          fcWhere = "lambda",
          fcAssigned = assignedNames body
        }
    lamEnv =
      Map.fromList
        [ (idName (paramName p), CtInfo (if paramComptime p then CTComptime else CTRuntime) (paramComptime p))
        | p <- ps
        ]
        `Map.union` env
checkExp _ _ _ = Right ()

-- | Verify that each comptime-annotated parameter receives a non-Runtime arg.
--   Skips polymorphic signatures (those whose comptime parameter types contain
--   type variables): the concrete types are only known after specialisation,
--   so polymorphic calls are deferred to the MAST-level check.
checkCallSite :: FunCtx -> CtEnv -> Id -> [Exp Id] -> Either String ()
checkCallSite fc env f args =
  case Map.lookup (idName f) (fcSigs fc) of
    Nothing -> Right ()
    Just sig
      | any (hasTypeVar . paramTy) (filter paramComptime (sigParams sig)) ->
          Right () -- polymorphic comptime param — defer to MAST-level check
      | otherwise ->
          mapM_ checkArg (zip (sigParams sig) args)
  where
    checkArg (param, arg) =
      when_ (paramComptime param && classifyExp fc env arg == CTRuntime) $
        "runtime value passed to comptime parameter '"
          ++ show (idName (paramName param))
          ++ "' of '"
          ++ show (idName f)
          ++ "'"

-- | A value of comptime-only type (string / integer) exists only at compile
-- time, so it is always comptime regardless of annotation.
isComptimeOnlyTy :: Ty -> Bool
isComptimeOnlyTy t = t == Prim.string || t == Prim.integer

-- | A function is effectively '-> comptime' if it is annotated so, or if its
-- return type is comptime-only.
effRetComptime :: Signature Id -> Bool
effRetComptime sig = sigRetComptime sig || maybe False isComptimeOnlyTy (sigReturn sig)

paramTy :: Param Id -> Ty
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

classifyExp :: FunCtx -> CtEnv -> Exp Id -> Ctness
classifyExp _ _ (Lit _) = CTComptime
classifyExp _ env (Var x) = maybe CTDeferred ctness (Map.lookup (idName x) env)
classifyExp fc env (TyExp e _) = classifyExp fc env e
classifyExp fc env (Call _ f args) = classifyCall fc env f args
classifyExp fc env (Con _ args) = combineCt (map (classifyExp fc env) args)
classifyExp fc env (Cond c t e) = combineCt (map (classifyExp fc env) [c, t, e])
classifyExp _ _ _ = CTDeferred

-- | Combine a list of Ctness values: all Comptime → Comptime;
--   any Runtime → Runtime; otherwise Deferred.
combineCt :: [Ctness] -> Ctness
combineCt cts
  | all (== CTComptime) cts = CTComptime
  | any (== CTRuntime) cts = CTRuntime
  | otherwise = CTDeferred

-- | Classify a function call result.
--   CTComptime iff the function is annotated '-> comptime' and ALL arguments
--   are CTComptime.  A non-comptime-annotated param in a '-> comptime' function
--   means "result is comptime when this arg happens to be comptime", so all args
--   must be checked, not just the comptime-annotated ones.
--   Never CTRuntime for calls — uncertain cases are deferred to MAST.
classifyCall :: FunCtx -> CtEnv -> Id -> [Exp Id] -> Ctness
classifyCall fc env f args =
  case Map.lookup (idName f) (fcSigs fc) of
    Nothing -> CTDeferred
    Just sig
      | effRetComptime sig && allArgsComptime ->
          CTComptime
      | otherwise ->
          CTDeferred
  where
    allArgsComptime = all (\arg -> classifyExp fc env arg == CTComptime) args

-----------------------------------------------------------------------
-- Helper
-----------------------------------------------------------------------

when_ :: Bool -> String -> Either String ()
when_ True msg = Left msg
when_ False _ = Right ()
