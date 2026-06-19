module Solcore.Backend.Specialise
  ( -- core API
    specialiseCompUnit,
    typeOfTcExp,
    -- testing API
    TVSubst (..),
    emptyTVSubst,
    (|->),
    runResolveMPTCTest,
  )
where

-- \* Specialisation
-- Create specialised versions of polymorphic and overloaded functions.
-- This is meant to be run on typed and defunctionalised code, so no higher-order functions.

import Common.Monad
import Common.Pretty
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Generics
import Data.List (intercalate, union, (\\))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Solcore.Backend.Mast
import Solcore.Desugarer.IfDesugarer (desugaredBoolTy)
import Solcore.Frontend.Pretty.ShortName
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax hiding (decls, name)
import Solcore.Frontend.TypeInference.Id (Id (..))
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcEnv (TcEnv (instEnv, typeTable), TypeInfo (..))
import Solcore.Frontend.TypeInference.TcUnify (typesDoNotUnify)
import Solcore.Primitives.Primitives hiding (integer)
import Solcore.Primitives.Primitives qualified as Prim

-- ** Specialisation state and monad

-- SpecState and SM are meant to be local to this module.
type Table a = Map.Map Name a

emptyTable :: Table a
emptyTable = Map.empty

type TcFunDef = FunDef Id

type TcExp = Exp Id

type Resolution = (Ty, TcFunDef)

data SpecState = SpecState
  { spResTable :: Table [Resolution],
    specTable :: Table TcFunDef,
    spTypeTable :: Table TypeInfo,
    spDataTable :: Table DataTy,
    spGlobalEnv :: TcEnv,
    splocalEnv :: Table Ty,
    spSubst :: TVSubst,
    spDebug :: Bool,
    spNS :: NameSupply
  }

type SM = StateT SpecState IO

getDebug :: SM Bool
getDebug = gets spDebug

whenDebug :: SM () -> SM ()
whenDebug m = do
  enabled <- getDebug
  when enabled m

debug :: [String] -> SM ()
debug msg = whenDebug (writes msg)

runSM :: Bool -> TcEnv -> SM a -> IO a
runSM debugp env m = evalStateT m (initSpecState debugp env)

-- prettys :: Pretty a => [a] -> String
-- prettys = render . brackets . commaSep . map ppr

-- | `withLocalState` runs a computation with a local state
-- local changes are discarded, with the exception of the `specTable` and name supply
withLocalState :: SM a -> SM a
withLocalState m = do
  saved <- get
  a <- m
  spTable <- gets specTable
  ns <- gets spNS
  put saved
  modify $ \s -> s {specTable = spTable, spNS = ns}
  return a

initSpecState :: Bool -> TcEnv -> SpecState
initSpecState debugp env =
  SpecState
    { spResTable = emptyTable,
      specTable = emptyTable,
      spTypeTable = typeTable env,
      spDataTable = Map.empty,
      spGlobalEnv = env,
      splocalEnv = emptyTable,
      spSubst = emptyTVSubst,
      spDebug = debugp,
      spNS = namePool
    }

{-
-- make type variables flexible by replacing them with metas
flex :: Ty -> Ty
flex (TyVar (TVar n)) = Meta (MetaTv n)
flex (TyCon cn tys) = TyCon cn (map flex tys)
flex t = t

-- make all type variables flexible in a syntactic construct
flexAll :: Data a => a -> a
flexAll = everywhere (mkT flex)
-}

-- | A signature forall tvs . ctx => t is considered ambiguous if a type variable
-- in tvs neither appears in the function type nor is reachable from the function
-- type's variables via the constraint graph.  Uses the same closure-based strategy
-- as TcStmt.ambiguous so that a constraint like `abs:Typedef(rep)` is not flagged
-- when `rep` appears in the type (abs is reachable through the constraint).
-- Returns the list of ambiguous variables.
ambiguousVarsInSig :: (HasTV a) => Signature a -> [Tyvar]
-- ambiguousVarsInSig sig = sigVars sig \\ freetv (sigParams sig, sigReturn sig)
ambiguousVarsInSig sig =
  sigVars sig \\ (tyVars `union` freetv (constraintClosure ps tyVars))
  where
    ps = sigContext sig
    tyVars = freetv (sigParams sig, sigReturn sig)
    reachable preds vs = [p | p <- preds, any (`elem` vs) (freetv p)]
    constraintClosure preds vs
      | all (`elem` vs) (freetv (reachable preds vs)) = reachable preds vs
      | otherwise = constraintClosure preds (freetv (reachable preds vs))

addSpecialisation :: Name -> TcFunDef -> SM ()
addSpecialisation name fd = modify $ \s -> s {specTable = Map.insert name fd (specTable s)}

lookupSpecialisation :: Name -> SM (Maybe TcFunDef)
lookupSpecialisation name = gets (Map.lookup name . specTable)

reportAmbiguousVars :: Name -> Signature Id -> SM ()
reportAmbiguousVars name sig = do
  let vars = ambiguousVarsInSig sig
  let scheme = schemeOfTcSignature sig
  unless (null vars) $
    nopanics
      [ "Error: function ",
        pretty name,
        " cannot be specialised because it has an ambiguous type:\n   ",
        pretty scheme,
        "\n variables: ",
        prettys vars,
        "\n do not occur in the argument/result types."
      ]

addResolution :: Name -> Ty -> TcFunDef -> SM ()
addResolution name ty fun = do
  -- debug ["+ addResolution ", pretty name, "@", pretty ty, " |-> ", shortName fun]
  reportAmbiguousVars name (funSignature fun)
  modify $ \s -> s {spResTable = Map.insertWith (++) name [(ty, fun)] (spResTable s)}

addDefaultResolution :: Name -> Ty -> TcFunDef -> SM ()
addDefaultResolution name ty fun = do
  reportAmbiguousVars name (funSignature fun)
  modify $ \s -> s {spResTable = Map.insertWith (\new old -> old ++ new) name [(ty, fun)] (spResTable s)}

lookupResolution :: Name -> Ty -> SM (Maybe (TcFunDef, Ty, TVSubst))
lookupResolution name ty = gets (Map.lookup name . spResTable) >>= findMatch ty
  where
    str :: (Pretty a) => a -> String
    str = pretty
    findMatch :: Ty -> Maybe [Resolution] -> SM (Maybe (TcFunDef, Ty, TVSubst))
    findMatch etyp (Just res) = do
      debug ["|> findMatch ", pretty etyp, " in ", prettyResolutions res]
      firstMatch etyp res
    findMatch _ Nothing = return Nothing
    firstMatch :: Ty -> [Resolution] -> SM (Maybe (TcFunDef, Ty, TVSubst))
    firstMatch _ [] = return Nothing
    firstMatch etyp ((t, e) : rest)
      | Right subst <- specmgu t etyp = do
          -- TESTME: match is to weak for MPTC, but isn't mgu too strong?
          debug ["< lookupRes - match found for ", str name, ": ", str t, " ~ ", str etyp, " => ", str subst]
          return (Just (e, t, subst))
      | otherwise = firstMatch etyp rest

getSpSubst :: SM TVSubst
getSpSubst = gets spSubst

putSpSubst :: TVSubst -> SM ()
putSpSubst subst = modify $ \s -> s {spSubst = subst}

extSpSubst :: TVSubst -> SM ()
extSpSubst subst = modify $ \s -> s {spSubst = spSubst s <> subst}

atCurrentSubst :: (HasTV a) => a -> SM a
atCurrentSubst a = flip applytv a <$> getSpSubst

addData :: DataTy -> SM ()
addData dt = modify (\s -> s {spDataTable = Map.insert (dataName dt) dt (spDataTable s)})

spNewName :: SM Name
spNewName = do
  s <- get
  let (n, ns) = newName (spNS s)
  put s {spNS = ns}
  pure (addPrefix "_" n)

-- data Name = Name String | QualName Name String
addPrefix :: String -> Name -> Name
addPrefix p (Name s) = Name (p ++ s)
addPrefix p (QualName q s) = QualName q (p ++ s)

-------------------------------------------------------------------------------

specialiseCompUnit :: CompUnit Id -> Bool -> TcEnv -> IO MastCompUnit
specialiseCompUnit compUnit debugp env = runSM debugp env do
  addGlobalResolutions compUnit
  topDecls <- concat <$> forM (contracts compUnit) specialiseTopDecl
  let specResult = compUnit {contracts = topDecls}
  return (toMastCompUnit specResult)

addGlobalResolutions :: CompUnit Id -> SM ()
addGlobalResolutions compUnit = forM_ (contracts compUnit) addDeclResolutions

addDeclResolutions :: TopDecl Id -> SM ()
addDeclResolutions (TInstDef inst) = addInstResolutions inst
addDeclResolutions (TFunDef fd) = addFunDefResolution fd
addDeclResolutions (TDataDef dt) = addData dt
addDeclResolutions (TMutualDef decls) = forM_ decls addDeclResolutions
addDeclResolutions _ = return ()

addInstResolutions :: Instance Id -> SM ()
addInstResolutions inst = forM_ (instFunctions inst) (addMethodResolution (instDefault inst) (instName inst) (mainTy inst))

specialiseTopDecl :: TopDecl Id -> SM [TopDecl Id]
specialiseTopDecl (TContr (Contract name args decls)) = withLocalState do
  addContractResolutions (Contract name args decls)
  -- Runtime code
  runtimeDecls <- withLocalState do
    forM_ entries specEntry
    getSpecialisedDecls
  -- Deployer code
  modify (\st -> st {specTable = emptyTable})
  mStart <- specEntryOpt deployerName
  deployDecls <- case mStart of
    Just {} -> do
      depDecls <- getSpecialisedDecls
      -- use mutual to group constructor with its dependencies
      pure [CMutualDecl depDecls]
    Nothing -> pure []
  return [TContr (Contract name args (deployDecls ++ runtimeDecls))]
  where
    entries = ["main"] -- Eventually all public methods
    getSpecialisedDecls :: SM [ContractDecl Id]
    getSpecialisedDecls = do
      st <- gets specTable
      dt <- gets spDataTable
      let dataDecls = map (CDataDecl . snd) (Map.toList dt)
      let funDecls = map (CFunDecl . snd) (Map.toList st)
      pure (dataDecls ++ funDecls)
specialiseTopDecl d@TDataDef {} = pure [d]
-- Drop all toplevel decls that are not contracts - we do not need them anymore
specialiseTopDecl _ = pure []

specEntry :: Name -> SM (Maybe Name)
specEntry name = do
  mres <- specEntryOpt name
  when (null mres) $ warns ["!! Warning: no resolution found for ", show name]
  pure mres

-- | Like 'specEntry' but silently returns Nothing when the name is absent.
-- Use for optional entry points (e.g. deployer) that may not exist when
-- contract dispatch generation is disabled.
specEntryOpt :: Name -> SM (Maybe Name)
specEntryOpt name = withLocalState do
  let anytype = TyVar (TVar (Name "any"))
  mres <- lookupResolution name anytype
  case mres of
    Just (fd, ty, subst) -> do
      debug ["< resolution: ", show name, " : ", pretty ty, "@", pretty subst]
      Just <$> specFunDef fd
    Nothing -> pure Nothing

addContractResolutions :: Contract Id -> SM ()
addContractResolutions (Contract _name _args cdecls) = do
  forM_ cdecls addCDeclResolution

addCDeclResolution :: ContractDecl Id -> SM ()
addCDeclResolution (CFunDecl fd) = addFunDefResolution fd
addCDeclResolution (CDataDecl dt) = addData dt
addCDeclResolution (CMutualDecl decls) = forM_ decls addCDeclResolution
addCDeclResolution _ = return ()

addFunDefResolution :: FunDef Id -> SM ()
addFunDefResolution fd = do
  let sig = funSignature fd
  let name = sigName sig
  let funType = typeOfTcFunDef fd
  addResolution name funType fd
  debug ["+ addDeclResolution: ", show name, " : ", pretty funType]

addMethodResolution :: Bool -> Name -> Ty -> TcFunDef -> SM ()
addMethodResolution isDefault cname ty fd = do
  let sig = funSignature fd
  let name = sigName sig
  let qname = case name of
        QualName {} -> name
        Name s -> QualName cname s
  let name' = specName qname [ty]
  let funType = typeOfTcFunDef fd
  let fd' = FunDef (funIsPublic fd) sig {sigName = name'} (funDefBody fd)
  if isDefault
    then addDefaultResolution qname funType fd'
    else addResolution qname funType fd'
  debug ["+ addMethodResolution: ", show qname, " / ", show name', " : ", pretty funType]

-- | `specExp` specialises an expression to given type
specExp :: TcExp -> Ty -> SM TcExp
specExp (Call Nothing i args) ty = do
  -- debug ["> specExp (Call): ", pretty e, " : ", pretty (idType i), " ~> ", pretty ty]
  (i', args') <- specCall i args ty
  let e' = Call Nothing i' args'
  -- debug ["< specExp (Call): ", pretty e']
  return e'
specExp e@(Con i es) ty = do
  debug ["> specConApp: ", pretty e, " : ", pretty (typeOfTcExp e), " ~> ", pretty ty]
  (i', es') <- specConApp i es ty
  let e' = Con i' es'
  return e'
specExp (Cond e1 e2 e3) ty = do
  e1' <- specExp e1 desugaredBoolTy
  e2' <- specExp e2 ty
  e3' <- specExp e3 ty
  pure (Cond e1' e2' e3')
specExp (Var (Id n _t)) ty = pure (Var (Id n ty))
specExp e@(FieldAccess _me _fld) _ty = error ("Specialise: FieldAccess not implemented for" ++ pretty e)
specExp (TyExp e1 _) ty = specExp e1 ty
specExp e@Lit {} _ = pure e
specExp e _ = do
  warns
    [ "! specExp: don't know how to handle: ",
      show e,
      "\n  Defaulting to atCurrentSubst"
    ]
  atCurrentSubst e

specConApp :: Id -> [TcExp] -> Ty -> SM (Id, [TcExp])
-- specConApp i@(Id n conTy) [] ty = pure (i, [])
specConApp i@(Id _n conTy) args ty = do
  subst <- getSpSubst
  let argTypes = map typeOfTcExp args
  let argTypes' = applytv subst argTypes
  let i' = applytv subst i
  let typedArgs = zip args argTypes'
  args' <- forM typedArgs (uncurry specExp)
  -- Unify the constructor's result type (from the current subst-applied conTy)
  -- with the target type ty.  This resolves phantom type parameters that do
  -- not appear in the constructor arguments (e.g. the 'ty' in
  -- ABIDecoder(ty, reader) = ABIDecoder(reader)), whose type var may differ
  -- from the caller's declared variable because the type checker used a
  -- fresh meta-variable and did not fully substitute it before emitting the
  -- typed AST.
  let resultConTy = applytv subst (resultTy conTy)
  case specmgu resultConTy ty of
    Right phi -> extSpSubst phi
    Left _ -> return ()
  let conTy' = foldr (:->) ty argTypes'
  debug ["> specConApp: ", prettyId i, " : ", pretty conTy, " ~> ", prettyId i', " : ", pretty conTy']
  debug ["< specConApp: ", prettyConApp i args, " ~> ", prettyConApp i' args']
  return (i', args')
  where
    -- Extract the result type of a function type (strip all arrows)
    resultTy (_ :-> rest) = resultTy rest
    resultTy t = t

-- | Specialise a function call
-- given actual arguments and the expected result type
-- Compiler builtins that are monomorphic and have no function body to
-- specialise.  Pass through with args specialised at their declared types.
-- Derived from Primitives.integerPrimNames (single source of truth shared
-- with MastEval.builtinPureFuns).
comptimeBuiltins :: [Name]
comptimeBuiltins = integerPrimNames

specCall :: Id -> [TcExp] -> Ty -> SM (Id, [TcExp])
specCall i@(Id (Name "revertLit") _) args _ = pure (i, args)
specCall (Id (QualName (Name "std") "revertLit") ty) args _ = pure (Id (Name "revertLit") ty, args)
-- Int.fromInteger coercion: resolve to the appropriate primitive based on result type.
-- integer -> word    becomes wordFromInteger (handled by MastEval)
-- integer -> integer is identity (handled by MastEval)
specCall (Id (QualName (Name "Int") "fromInteger") ty) args _ = do
  args' <- mapM (\a -> specExp a (typeOfTcExp a)) args
  s <- getSpSubst
  let resultTy = snd (splitTy (applytv s ty))
  if resultTy == word
    then pure (Id (Name "wordFromInteger") (Prim.integer :-> word), args')
    else pure (Id (QualName (Name "Int") "fromInteger") (Prim.integer :-> resultTy), args')
-- Str.fromString coercion: resolve based on result type.
-- string -> memory(string) becomes memStringFromLit (lowered per-literal by EmitHull)
-- string -> string         is identity (folded by MastEval)
specCall (Id (QualName (Name "Str") "fromString") ty) args _ = do
  args' <- mapM (\a -> specExp a (typeOfTcExp a)) args
  s <- getSpSubst
  let resultTy = snd (splitTy (applytv s ty))
  if resultTy == Prim.memString
    then pure (Id Prim.memStringFromLitName (Prim.string :-> Prim.memString), args')
    else pure (Id (QualName (Name "Str") "fromString") (Prim.string :-> resultTy), args')
specCall i args _ty
  | idName i `elem` comptimeBuiltins = do
      args' <- mapM (\a -> specExp a (typeOfTcExp a)) args
      pure (i, args')
specCall i args ty = do
  i' <- atCurrentSubst i
  ty' <- atCurrentSubst ty
  -- debug ["> specCall: ", pretty i', show args, " : ", pretty ty']
  let name = idName i'
  let argTypes = map typeOfTcExp args
  argTypes' <- atCurrentSubst argTypes
  let typedArgs = zip args argTypes'
  args' <- forM typedArgs (uncurry specExp)
  let funType = foldr (:->) ty' argTypes'
  debug ["> specCall: ", show name, " : ", pretty funType]
  mres <- lookupResolution name funType
  case mres of
    Just (fd, fty, phi) -> do
      debug ["< resolution: ", show name, "~>", shortName fd, " : ", pretty fty, "@", pretty phi]
      let varToVar = [(v, t) | (v, t) <- unTVSubst phi, isTyVar t]
      unless (null varToVar) $
        warns
          [ "Warning: call to ",
            show name,
            " resolved with ungrounded type variable(s): ",
            prettys (map snd varToVar),
            "\n  The intermediate type cannot be determined from this call site.",
            "\n  Expression: ",
            pretty (Call Nothing i args),
            "\n  This often occurs when a polymorphic-return function (e.g. `require`)",
            "\n  is passed directly to a polymorphic-argument function (e.g. `void`)."
          ]
      extSpSubst phi
      subst <- getSpSubst
      let ty'' = applytv subst fty
      ensureClosed ty'' (Call Nothing i args) subst
      name' <- specFunDef fd
      debug ["< specCall: ", pretty name', " : ", show ty'']
      args'' <- atCurrentSubst args'
      return (Id name' ty'', args'')
    Nothing -> do
      void $ panics ["! specCall: no resolution found for ", show name, " : ", pretty funType]
      return (i, args')

-- | `specFunDef` specialises a function definition
-- to the given type of the form `arg1Ty -> arg2Ty -> ... -> resultTy`
-- first lookup if a specialisation to the given type exists
-- if not, look for a resolution (definition matching the expected type)
-- create a new specialisation of it and record it in `specTable`
-- returns name of the specialised function
specFunDef :: TcFunDef -> SM Name
specFunDef fd0 = withLocalState do
  -- first, rename bound variables
  (fd, renamingSubst) <- renametv fd0
  let renaming = fromTVS renamingSubst
  let sig = funSignature fd
  let name = sigName sig
  let funType = typeOfTcFunDef fd
  let tvs = freetv funType
  subst <- renameSubst renaming <$> getSpSubst
  putSpSubst subst
  let tvs' = applytv subst (map TyVar tvs)
  debug ["> specFunDef ", pretty name, " : ", pretty funType, " tvs'=", prettys tvs', " subst=", pretty subst]
  let name' = specName name tvs'
  let ty' = applytv subst funType
  mspec <- lookupSpecialisation name'
  case mspec of
    Just _ -> return name'
    Nothing -> do
      let sig' = applytv subst (funSignature fd)
      -- add a placeholder first to break loops
      let placeholder = FunDef (funIsPublic fd) sig' []
      addSpecialisation name' placeholder
      -- Resolve MPTC extra type parameters from constraints.
      -- E.g. for  forall a rep. a:Generic(rep) => f(x:a)...
      -- when a is concrete (say Option(uint256)) but rep is still free,
      -- look up a Generic method for Option(uint256) to determine rep.
      resolveMPTCFromPreds (sigContext sig')
      body' <- specBody (funDefBody fd)
      let fd' = FunDef (funIsPublic fd) sig' {sigName = name'} body'
      debug ["+ specFunDef: adding specialisation ", show name', " : ", pretty ty']
      addSpecialisation name' fd'
      return name'

specBody :: [Stmt Id] -> SM [Stmt Id]
specBody = mapM specStmt

-- Recover concrete bindings for MPTC extra type parameters that are still free
-- after the call-site types have been applied.
--
-- A constraint  InCls cls mainTy [extra]  has two groups of type parameters:
--   1. mainTy: the "self" type, determined directly from the call-site argument
--              or return type (e.g. the concrete type passed to a polymorphic fn)
--   2. extras: additional class parameters that may not appear in the function
--              type at all (phantom parameters), so they cannot be read off the
--              call site; they must be inferred from the instance.
--
-- The guard  null (freetv mainTy') && any (not . null . freetv) extras'
-- captures exactly the interesting case: mainTy is fully concrete (we know
-- which instance to look at) but at least one extra is still open (we need to
-- read the instance to close it).  When all extras are already concrete, or
-- when mainTy itself is still abstract, there is nothing for this function to do.
resolveMPTCFromPreds :: [Pred] -> SM ()
resolveMPTCFromPreds preds = do
  subst <- getSpSubst
  forM_ preds $ \pred' -> case pred' of
    InCls clsName mainTy1 extras -> do
      let mainTy' = applytv subst mainTy1
          extras' = map (applytv subst) extras
      when (null (freetv mainTy') && any (not . null . freetv) extras') $
        tryResolveMPTC clsName mainTy' extras'
    _ -> return ()

-- | Testing entry point: run resolveMPTCFromPreds in an isolated SM environment.
-- Sets the initial substitution to @s0@, runs @resolveMPTCFromPreds preds@, and
-- returns the resulting substitution.  The @TcEnv@ supplies the instance table.
runResolveMPTCTest :: TcEnv -> TVSubst -> [Pred] -> IO TVSubst
runResolveMPTCTest env s0 preds = runSM False env $ do
  putSpSubst s0
  resolveMPTCFromPreds preds
  getSpSubst

-- tryResolveMPTC deduces concrete types for the extra parameters of a
-- multi-parameter type class (MPTC) constraint when the "self" (main) type is
-- already fully concrete but some extra parameters are still free type variables.
--
-- The strategy is to search the type-checker's instance environment for the
-- unique instance of C whose main type matches mainTy', then read the extras
-- directly from the instance head.
--
-- Soundness: the instance environment is produced and verified by the type
-- checker.  Every entry  (ctx :=> InCls C instMainTy instExtras)  is a
-- certified fact: the type checker has established that instMainTy is an
-- instance of C with extra parameters instExtras.  When specmatch confirms that
-- instMainTy matches the concrete mainTy', we have that instExtras is a valid
-- deduction, it recovers exactly what the type checker already knew.
--
-- Two guards prevent incorrect extensions:
--
--   1. all (null . freetv) concreteExtras
--        Only propagate fully ground types.  If the matched instance is itself
--        parametric in a variable not pinned by mainTy', we cannot determine a
--        unique concrete type for the extra and skip to avoid a partial binding.
--
--   2. specmatch extra concrete returns Left
--        If a call-site extra is already bound to a different type, specmatch
--        fails and the no-op branch preserves the earlier authoritative binding.
--
-- Note: specmatch (not specmgu) is used throughout because both operations are
-- inherently one-directional.  mainTy' is guaranteed ground (no free variables)
-- by the caller, so the instance head is the pattern and the call-site type is
-- the fixed target; there is no need to bind variables on the right-hand side.
-- Using matching rather than unification makes this asymmetry explicit and
-- rules out accidental bindings on the call-site side.
tryResolveMPTC :: Name -> Ty -> [Ty] -> SM ()
tryResolveMPTC clsName mainTy' extras = do
  -- The instance environment was produced by the type checker and maps each
  -- class name to its list of verified instances.
  instTable <- gets (instEnv . spGlobalEnv)
  let clsInsts = fromMaybe [] (Map.lookup clsName instTable)
  forM_ clsInsts $ \inst ->
    case inst of
      -- Only class constraint heads are relevant; skip equality constraints.
      (q :=> InCls _ instMainTy instExtras) ->
        -- Match the instance's main type (pattern) against the call-site's
        -- concrete main type (target).  specmatch binds the instance's
        -- quantified type variables (TVar) as needed and never touches
        -- variables on the target side.  Failure means this instance is for
        -- a different concrete type; skip silently.
        case specmatch instMainTy mainTy' of
          Left _ -> return ()
          Right phi -> do
            -- anfInstance normalises instances with extras by replacing each
            -- concrete extra with a fresh variable bound by an equality pred:
            --   C t [e1,e2]  =>  [v1:~:e1, v2:~:e2] :=> C t [v1,v2]
            -- After matching instMainTy against mainTy' we must also resolve
            -- these equality constraints (with phi applied) to recover the
            -- actual concrete extras; otherwise instExtras still contains
            -- the fresh TyVars and looks abstract.
            let appliedQ = map (applytv phi) q
                eqSubst =
                  TVSubst
                    [ (v, t)
                      | (TyVar v :~: t) <- appliedQ,
                        null (freetv t)
                    ]
                    <> TVSubst
                      [ (v, t)
                        | (t :~: TyVar v) <- appliedQ,
                          null (freetv t)
                      ]
                phi' = phi <> eqSubst
            debug ["  tryResolve match: phi=", pretty phi, " q=", show q, " appliedQ=", show appliedQ, " eqSubst=", pretty eqSubst, " phi'=", pretty phi', " instExtras=", show instExtras]
            -- Substitute to obtain the concrete extra types for this instance.
            let concreteExtras = map (applytv phi') instExtras
            debug ["  concreteExtras=", show concreteExtras, " allClosed=", show (all (null . freetv) concreteExtras)]
            -- If any extra is still abstract, the instance is parametric in a
            -- way that mainTy' alone does not determine; skip to stay sound.
            when (all (null . freetv) concreteExtras) $
              forM_ (zip extras concreteExtras) $ \(extra, concrete) ->
                -- Match the call-site free extra (pattern) against the
                -- instance's concrete extra (target).  If extra is already
                -- bound to the same type this is a no-op (phi = mempty).
                -- If bound to a different type specmatch returns Left and the
                -- earlier authoritative binding is preserved.
                case specmatch extra concrete of
                  Left _ -> return ()
                  Right phi2 -> do
                    debug ["  extSpSubst phi2=", pretty phi2]
                    extSpSubst phi2
      _ -> return ()

{-
ensureSimple ty' stmt subst = case ty' of
    TyVar _ -> panics [ "specStmt(",pretty stmt,"): polymorphic return type: "
                      ,  pretty ty', " subst=", pretty subst]
    _ :-> _ -> panics [ "specStmt(",pretty stmt,"): function return type: "
                      , pretty ty'
                      ,"\nIn:\n", show stmt
                      ]
    _ -> return ()
-}

-- | `ensureClosed` checks that a type is closed, i.e. has no free type variables
ensureClosed :: (Pretty a) => Ty -> a -> TVSubst -> SM ()
ensureClosed ty ctxt subst = do
  let tvs = freetv ty
  unless (null tvs) $
    nopanics
      [ "Error: cannot specialise ",
        pretty ctxt,
        "\n",
        "  Type variable(s) ",
        prettys tvs,
        " remain unresolved in type ",
        pretty ty,
        "\n",
        "  This usually means a polymorphic return value is passed to another\n",
        "  polymorphic function without any concrete type context to resolve\n",
        "  the intermediate type (e.g. void(require(...))).\n",
        "  Substitution: ",
        pretty subst
      ]

{-
  let mvs = mv ty
  unless (null tvs) $ panics ["spec(", pretty ctxt,"): free meta vars in ", pretty ty, ": ", show mvs
                             , " @ subst=", pretty subst]
-}

specStmt :: Stmt Id -> SM (Stmt Id)
specStmt stmt@(Return e) = do
  subst <- getSpSubst
  let ty = typeOfTcExp e
  let ty' = applytv subst ty
  ensureClosed ty' stmt subst
  -- debug ["> specExp (Return): ", pretty e," : ", pretty ty, " ~> ", pretty ty']
  e' <- specExp e ty'
  -- debug ["< specExp (Return): ", pretty e']
  return $ Return e'
specStmt (Match exps alts) = specMatch exps alts
specStmt stmt@(Var i := e) = do
  subst <- getSpSubst
  i' <- atCurrentSubst i
  let ty' = idType i'
  debug
    [ "> specStmt (:=): ",
      pretty i,
      " : ",
      pretty (idType i),
      " @ ",
      pretty subst,
      "~>'",
      pretty ty'
    ]
  ensureClosed ty' stmt subst
  e' <- specExp e ty'
  debug ["< specExp (:=): ", pretty e']
  return $ Var i' := e'
specStmt stmt@(Let ct i mty mexp) = do
  subst <- getSpSubst
  debug ["> specStmt (Let): ", pretty i, " : ", pretty (idType i), " mty=", maybe "Nothing" pretty mty, " @ ", pretty subst]
  i' <- atCurrentSubst i
  let ty' = idType i'
  case mexp of
    Nothing -> do
      ensureClosed ty' stmt subst
      mty' <- atCurrentSubst mty
      return $ Let ct i' mty' Nothing
    Just e ->
      if null (freetv ty')
        then do
          ensureClosed ty' stmt subst
          mty' <- atCurrentSubst mty
          e' <- specExp e ty'
          return $ Let ct i' mty' (Just e')
        else do
          -- ty' still has free type variables.  Try to resolve them from the
          -- explicit type annotation (mty), which went through elabFunDef and
          -- has the original forall-bound names (_g2, _h2, …) that are already
          -- in the current substitution.  This handles the case where the type
          -- checker introduced a fresh meta-variable for a phantom type
          -- parameter (e.g. the `ty` in `ABIDecoder(ty, reader)`) that was
          -- never renamed to the function's forall variable.
          ty_for_spec <- case mty of
            Just ann -> do
              ann' <- atCurrentSubst ann
              case specmgu ty' ann' of
                Right phi -> extSpSubst phi >> atCurrentSubst ty'
                Left _ -> return ty'
            Nothing -> return ty'
          e' <- specExp e ty_for_spec
          subst' <- getSpSubst
          i'' <- atCurrentSubst i
          let ty'' = idType i''
          ensureClosed ty'' stmt subst'
          mty' <- atCurrentSubst mty
          return $ Let ct i'' mty' (Just e')
specStmt (Block body) =
  Block <$> specBody body
specStmt (StmtExp e) = do
  ty <- atCurrentSubst (typeOfTcExp e)
  -- replace all type variables with unit - the value is dropped anyway

  let groundTy (TyVar _) = unit
      groundTy (TyCon n tys) = TyCon n (map groundTy tys)
      groundTy (a :-> b) = groundTy a :-> groundTy b
      groundTy t = t

  e' <- specExp e (groundTy ty)
  return $ StmtExp e'
specStmt (For initStmt cond post body) = do
  initStmt' <- specStmt initStmt
  cond' <- specExp cond desugaredBoolTy
  post' <- specStmt post
  body' <- specBody body
  return $ For initStmt' cond' post' body'
specStmt (Asm ys) = pure (Asm ys)
specStmt EmptyStmt = pure EmptyStmt
specStmt stmt = errors ["specStmt not implemented for: ", show stmt]

specMatch :: [Exp Id] -> [([Pat Id], [Stmt Id])] -> SM (Stmt Id)
specMatch exps alts = do
  -- subst <- getSpSubst
  -- debug ["> specMatch, scrutinee: ", pretty exps, " @ ", pretty subst]
  exps' <- specScruts exps
  alts' <- forM alts specAlt
  -- debug ["< specMatch, alts': ", show alts']
  return $ Match exps' alts'
  where
    specAlt (pat, body) = do
      -- debug ["specAlt, pattern: ", show pat]
      -- debug ["specAlt, body: ", show body]
      body' <- specBody body
      pat' <- mapM specPat =<< atCurrentSubst pat
      return (pat', body')
    -- Specialize function calls inside PExp patterns.
    -- Other pattern forms only need type substitution (handled by atCurrentSubst).
    specPat :: Pat Id -> SM (Pat Id)
    specPat (PExp e) = do
      ty <- atCurrentSubst (typeOfTcExp e)
      PExp <$> specExp e ty
    specPat p = pure p
    specScruts = mapM specScrut
    specScrut e = do
      ty <- atCurrentSubst (typeOfTcExp e)
      e' <- specExp e ty
      -- debug ["specScrut: ", show e, " to ", pretty ty, " ~>", show e']
      return e'

specName :: Name -> [Ty] -> Name
specName n [] = Name $ flattenQual n
specName n ts = Name $ flattenQual n ++ "$" ++ intercalate "_" (map mangleTy ts)

flattenQual :: Name -> String
flattenQual (Name n) = n
flattenQual (QualName n s) = flattenQual n ++ "_" ++ s

mangleTy :: Ty -> String
mangleTy (TyVar (TVar (Name n))) = n
mangleTy (Meta (MetaTv (Name n))) = n
mangleTy (TyCon (Name "()") []) = "unit"
mangleTy (TyCon (Name n) []) = n
mangleTy (TyCon (Name n) ts) = n ++ "L" ++ intercalate "_" (map mangleTy ts) ++ "J"
mangleTy ty = error ("mangleTy - unexpected type: " ++ show ty)

prettyId :: Id -> String
prettyId = render . pprId

pprId :: Id -> Doc
pprId (Id n t@TyVar {}) = ppr n <> text "@" <> ppr t
pprId (Id n t@(TyCon _cn [])) = ppr n <> "@" <> ppr t
pprId (Id n t) = ppr n <> text "@" <> parens (ppr t)

pprConApp :: Id -> [TcExp] -> Doc
pprConApp i args = pprId i <> brackets (commaSepList args)

prettyConApp :: Id -> [TcExp] -> String
prettyConApp i args = render (pprConApp i args)

typeOfTcExp :: TcExp -> Ty
typeOfTcExp (Var i) = idType i
typeOfTcExp (Con i []) = idType i
typeOfTcExp e@(Con i args) = go (idType i) args
  where
    go ty [] = ty
    go (_ :-> u) (_ : as) = go u as
    go _ _ = error $ "typeOfTcExp: " ++ show e
typeOfTcExp (Lit (IntLit _)) = Prim.integer
typeOfTcExp (Lit (StrLit _)) = string
typeOfTcExp expr@(Call Nothing i args) = applyTo args funTy
  where
    funTy = idType i
    applyTo [] ty = ty
    applyTo (_ : as) (_ :-> u) = applyTo as u
    applyTo _ _ =
      error $
        concat
          [ "apply ",
            pretty i,
            " : ",
            pretty funTy,
            "to",
            show $ map pretty args,
            "\nIn:\n",
            show expr
          ]
typeOfTcExp (Lam args _body (Just tb)) = funtype tas tb
  where
    tas = map typeOfTcParam args
typeOfTcExp (Cond _ _ e) = typeOfTcExp e
typeOfTcExp (TyExp _ ty) = ty
typeOfTcExp e = error $ "typeOfTcExp: " ++ show e

typeOfTcParam :: Param Id -> Ty
typeOfTcParam (Typed _ i _t) = idType i -- seems better than t - see issue #6
typeOfTcParam (Untyped _ i) = idType i

typeOfTcSignature :: Signature Id -> Ty
typeOfTcSignature sig = funtype (map typeOfTcParam $ sigParams sig) returnType
  where
    returnType = case sigReturn sig of
      Just t -> t
      Nothing -> error ("no return type in signature of: " ++ show (sigName sig))

schemeOfTcSignature :: Signature Id -> Scheme
schemeOfTcSignature sig@(Signature vs ps _n args _ (Just rt) _) =
  case mapM getType args of
    Just ts -> Forall vs (ps :=> (funtype ts rt))
    Nothing -> error $ unwords ["Invalid instance member signature:", pretty sig]
  where
    getType (Typed _ _ t) = Just t
    getType _ = Nothing
schemeOfTcSignature sig = error ("no return type in signature of: " ++ show (sigName sig))

typeOfTcFunDef :: TcFunDef -> Ty
typeOfTcFunDef (FunDef _ sig _) = typeOfTcSignature sig

pprRes :: Resolution -> Doc
-- type Resolution = (Ty, FunDef Id)
pprRes (ty, fd) = ppr ty <+> text ":" <+> text (shortName fd)

prettyResolutions :: [Resolution] -> String
prettyResolutions = render . brackets . commaSep . map pprRes

-- instance Pretty (Ty, FunDef Id) where
--  ppr = pprRes

isTyVar :: Ty -> Bool
isTyVar (TyVar _) = True
isTyVar _ = False

specmgu :: Ty -> Ty -> Either String TVSubst
specmgu (TyCon n ts) (TyCon n' ts')
  | n == n' && length ts == length ts' =
      specsolve (zip ts ts') mempty
specmgu (TyVar v) t = varBind v t
specmgu t (TyVar v) = varBind v t
specmgu t1 t2 = typesDoNotUnify t1 t2

-- | One-directional matching: find a substitution @phi@ such that
-- @applytv phi pat == tgt@, binding only variables that appear in @pat@.
-- Unlike 'specmgu', a type variable on the right-hand side (@tgt@) is never
-- bound; if @tgt@ contains a free variable the match fails.  This is the
-- correct operation for instance-head lookup: the instance type is the pattern
-- (may contain quantified variables) and the call-site type is the target
-- (must be fully concrete).
specmatch :: Ty -> Ty -> Either String TVSubst
specmatch (TyCon n ts) (TyCon n' ts')
  | n == n' && length ts == length ts' =
      matchsolve (zip ts ts') mempty
specmatch (TyVar v) t = varBind v t
specmatch t1 t2 = typesDoNotUnify t1 t2

matchsolve :: [(Ty, Ty)] -> TVSubst -> Either String TVSubst
matchsolve [] s = pure s
matchsolve ((pat, tgt) : rest) s =
  do
    s1 <- specmatch (applytv s pat) tgt
    s2 <- matchsolve rest s1
    pure (s2 <> s1)

varBind :: (MonadError String m) => Tyvar -> Ty -> m TVSubst
varBind v t
  | t == TyVar v = return mempty
  | v `elem` freetv t = infiniteTyErr v t
  | otherwise = do
      return (v |-> t)
  where
    infiniteTyErr w u =
      throwError $
        unwords
          [ "Cannot construct the infinite type:",
            pretty w,
            "~",
            pretty u
          ]

specsolve :: [(Ty, Ty)] -> TVSubst -> Either String TVSubst
specsolve [] s = pure s
specsolve ((t1, t2) : ts) s =
  do
    s1 <- specmgu (applytv s t1) (applytv s t2)
    s2 <- specsolve ts s1
    pure (s2 <> s1)

newtype TVSubst
  = TVSubst {unTVSubst :: [(Tyvar, Ty)]}
  deriving (Eq, Show)

emptyTVSubst :: TVSubst
emptyTVSubst = TVSubst []

-- composition operators

instance Semigroup TVSubst where
  s1 <> s2 = TVSubst (outer ++ inner)
    where
      outer = [(u, applytv s1 t) | (u, t) <- unTVSubst s2]
      inner = [(v, t) | (v, t) <- unTVSubst s1, v `notElem` dom2]
      dom2 = map fst (unTVSubst s2)

instance Monoid TVSubst where
  mempty = emptyTVSubst

(|->) :: Tyvar -> Ty -> TVSubst
u |-> t = TVSubst [(u, t)]

instance Pretty TVSubst where
  ppr = braces . commaSep . map go . unTVSubst
    where
      go (v, t) = ppr v <+> text "|->" <+> ppr t

class (Data a) => HasTV a where
  applytv :: TVSubst -> a -> a
  applytv s = everywhere (mkT (applytv @Ty s))

  freetv :: a -> [Tyvar] -- free variables
  freetv = everything (<>) (mkQ mempty (freetv @Ty))

  renametv :: a -> SM (a, TVSubst)
  renametv a = pure (a, mempty)

instance HasTV Ty where
  applytv (TVSubst s) t@(TyVar v) =
    maybe t id (lookup v s)
  applytv s (TyCon n ts) =
    TyCon n (applytv s ts)
  applytv _ t = t

  freetv (TyVar v@(TVar _)) = [v]
  freetv (TyCon _ ts) = freetv ts
  freetv _ = []

instance (HasTV a) => HasTV [a] where
  applytv s = map (applytv s)
  freetv = foldr (union . freetv) mempty

instance (HasTV a) => HasTV (Maybe a) where
  applytv s = fmap (applytv s)
  freetv = maybe [] freetv

instance (HasTV a, HasTV b) => HasTV (a, b) -- defaults

instance HasTV Pred -- uses default: freetv = everything (<>) (mkQ mempty (freetv @Ty))

{-
instance (HasTV a, HasTV b, HasTV c) => HasTV (a,b,c) where
  applytv s (z,x,y) = (applytv s z, applytv s x, applytv s y)
  freetv (z,x,y) = freetv z `union` freetv x `union` freetv y

instance (HasTV a, HasTV b) => HasTV (a,b) where
  applytv s (x,y) = (applytv s x, applytv s y)
  freetv (x,y) = freetv x `union` freetv y
-}

instance HasTV Id where
  applytv s (Id n t) = Id n (applytv s t)
  freetv (Id _ t) = freetv t

instance (HasTV a) => HasTV (Param a) -- defaults

instance (HasTV a) => HasTV (Exp a) -- defaults

instance (HasTV a) => HasTV (Stmt a) -- defaults

instance HasTV (Pat Id)

instance HasTV (Signature Id) where
  applytv s = everywhere (mkT (applytv @Ty s))
  freetv sig = (everything (<>) (mkQ mempty (freetv @Ty))) sig \\ sigVars sig
  renametv sig = do
    subst <- foldM addRenaming mempty (sigVars sig)
    pure (applytv subst sig, subst)

{-
data FunDef a
  = FunDef {
      funSignature :: Signature a
    , funDefBody :: [Stmt a]
    } deriving (Eq, Ord, Show, Data, Typeable)
-}

instance HasTV (FunDef Id) where
  freetv fd = (everything (<>) (mkQ mempty (freetv @Ty))) fd \\ sigVars (funSignature fd)
  renametv fd = do
    let sig = funSignature fd
    subst <- foldM addRenaming mempty (sigVars sig)
    let sig' = applytv subst sig
    let body' = applytv subst (funDefBody fd)
    pure (FunDef (funIsPublic fd) sig' body', subst)

addRenaming :: TVSubst -> Tyvar -> SM TVSubst
addRenaming b a = do
  fresh <- spNewName
  pure ((a |-> TyVar (TVar fresh)) <> b)

-- TODO: refactor - make renametv return TVRenaming; turn rename* into class methods

newtype TVRenaming
  = TVR {unTVR :: [(Tyvar, Tyvar)]}
  deriving (Eq, Show)

instance Pretty TVRenaming where
  ppr = braces . commaSep . map go . unTVR
    where
      go (v, t) = ppr v <+> text "|->" <+> ppr t

fromTVS :: TVSubst -> TVRenaming
fromTVS = TVR . map (fmap unTyVar) . unTVSubst
  where
    unTyVar (TyVar x) = x
    unTyVar t = error ("fromTVS: " ++ pretty t ++ "is not a type variable")

renameTV :: TVRenaming -> Tyvar -> Tyvar
renameTV (TVR r) v = fromMaybe v (lookup v r)

renameTy :: TVRenaming -> Ty -> Ty
renameTy r = everywhere (mkT (renameTV r))

renameSubst :: TVRenaming -> TVSubst -> TVSubst
renameSubst r = TVSubst . map rename . unTVSubst
  where
    rename (v, t) = (renameTV r v, renameTy r t)

-----------------------------------------------------------------------
-- Conversion from internal CompUnit Id to MastCompUnit
-----------------------------------------------------------------------

toMastCompUnit :: CompUnit Id -> MastCompUnit
toMastCompUnit (CompUnit imps ds) = MastCompUnit imps (map toMastTopDecl ds)

toMastTopDecl :: TopDecl Id -> MastTopDecl
toMastTopDecl (TContr c) = MastTContr (toMastContract c)
toMastTopDecl (TDataDef dt) = MastTDataDef dt
toMastTopDecl d = error $ "toMastTopDecl: unexpected " ++ show d

toMastContract :: Contract Id -> MastContract
toMastContract (Contract n _tyParams ds) = MastContract n (map toMastContractDecl ds)

toMastContractDecl :: ContractDecl Id -> MastContractDecl
toMastContractDecl (CDataDecl dt) = MastCDataDecl dt
toMastContractDecl (CFunDecl fd) = MastCFunDecl (toMastFunDef fd)
toMastContractDecl (CMutualDecl ds) = MastCMutualDecl (map toMastContractDecl ds)
toMastContractDecl d = error $ "toMastContractDecl: unexpected " ++ show d

toMastFunDef :: FunDef Id -> MastFunDef
toMastFunDef (FunDef _ sig body) =
  MastFunDef
    { mastFunName = sigName sig,
      mastFunParams = map toMastParam (sigParams sig),
      mastFunRetComptime = sigRetComptime sig,
      mastFunReturn = case sigReturn sig of
        Just t -> toMastTy t
        Nothing -> error $ "toMastFunDef: no return type for " ++ show (sigName sig),
      mastFunBody = toMastBody body
    }

toMastParam :: Param Id -> MastParam
toMastParam p = MastParam (idName i) (paramComptime p) (toMastTy (idType i))
  where
    i = getParamId p

getParamId :: Param Id -> Id
getParamId (Typed _ i _) = i
getParamId (Untyped _ i) = i

toMastTy :: Ty -> MastTy
toMastTy = tyToMast

toMastId :: Id -> MastId
toMastId (Id n t) = MastId n (toMastTy t)

toMastStmt :: Stmt Id -> MastStmt
toMastStmt (Var i := e) = MastAssign (toMastId i) (toMastExp e)
toMastStmt (Let ct i mty me) = MastLet ct (toMastId i) (fmap toMastTy mty) (fmap toMastExp me)
toMastStmt (StmtExp e) = MastStmtExp (toMastExp e)
toMastStmt (Return e) = MastReturn (toMastExp e)
toMastStmt (Match [scrutinee] alts) = MastMatch (toMastExp scrutinee) (map toMastAlt alts)
toMastStmt (Match es _) = error $ "toMastStmt: multi-scrutinee match should have been desugared: " ++ show es
toMastStmt (Asm ys) = MastAsm ys
toMastStmt (For initStmt cond postStmt body) =
  MastFor (toMastStmt initStmt) (toMastExp cond) (toMastStmt postStmt) (toMastBody body)
toMastStmt (Block body) = MastSeq (toMastBody body)
toMastStmt EmptyStmt = MastSeq []
toMastStmt s = error $ "toMastStmt: unexpected " ++ show s

toMastBody :: [Stmt Id] -> [MastStmt]
toMastBody = concatMap go
  where
    go (Block body) = toMastBody body
    go stmt = [toMastStmt stmt]

toMastAlt :: ([Pat Id], [Stmt Id]) -> MastAlt
toMastAlt ([p], body) = (toMastPat p, toMastBody body)
toMastAlt (ps, _) = error $ "toMastAlt: multi-pattern alt should have been desugared: " ++ show ps

toMastExp :: Exp Id -> MastExp
toMastExp (Var i) = MastVar (toMastId i)
toMastExp (Con i es) = MastCon (toMastId i) (map toMastExp es)
toMastExp (Lit l) = MastLit l
toMastExp (Call Nothing i es) = MastCall (toMastId i) (map toMastExp es)
toMastExp (TyExp e _) = toMastExp e
toMastExp (Cond e1 e2 e3) = MastCond (toMastExp e1) (toMastExp e2) (toMastExp e3)
toMastExp e = error $ "toMastExp: unexpected " ++ show e

toMastPat :: Pat Id -> MastPat
toMastPat (PVar i) = MastPVar (toMastId i)
toMastPat (PCon i ps) = MastPCon (toMastId i) (map toMastPat ps)
toMastPat PWildcard = MastPWildcard
toMastPat (PLit l) = MastPLit l
toMastPat (PExp e) = MastPExp (toMastExp e)
