module Solcore.Frontend.TypeInference.TcMonad where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Generics (Data, everything, extQ, mkQ)
import Data.List
import Data.List.NonEmpty qualified as N
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid (First (..))
import Data.Set qualified as Set
import Solcore.Diagnostics (CompilerError (..), Diagnostic (..), DiagnosticCode (..), Label (..), LabelStyle (..), Severity (..), SourceSpan, addDiagnosticNote, diagnosticCompilerError)
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Pipeline.Options (Option (..))
import System.TimeIt qualified as TimeIt
import Text.Printf

-- definition of type inference monad infrastructure

type TcM a = (StateT TcEnv (ExceptT CompilerError IO)) a

runTcM :: TcM a -> TcEnv -> IO (Either CompilerError (a, TcEnv))
runTcM m env = runExceptT (runStateT m env)

defaultM :: TcM a -> TcM (Maybe a)
defaultM m =
  do
    x <- m
    pure (Just x)
    `catchError` (\_ -> pure Nothing)

freshVar :: TcM MetaTv
freshVar =
  MetaTv <$> freshName

freshName :: TcM Name
freshName =
  do
    v <- incCounter
    pure (Name ("$" ++ show v))

incCounter :: TcM Int
incCounter = do
  c <- gets counter
  modify (\st -> st {counter = c + 1})
  pure c

askGeneratingDefs :: TcM Bool
askGeneratingDefs = gets generateDefs

setGeneratingDefs :: Bool -> TcM ()
setGeneratingDefs b =
  modify (\env -> env {generateDefs = b})

withNoGeneratingDefs :: TcM a -> TcM a
withNoGeneratingDefs m =
  do
    b <- askGeneratingDefs
    setGeneratingDefs False
    r <- m
    setGeneratingDefs b
    pure r

addUniqueType :: Name -> DataTy -> TcM ()
addUniqueType n dt =
  do
    modify (\st -> st {uniqueTypes = Map.insert n dt (uniqueTypes st)})
    checkDataType dt

lookupUniqueTy :: Name -> TcM (Maybe DataTy)
lookupUniqueTy n =
  (Map.lookup n) <$> gets uniqueTypes

isUniqueTyName :: Name -> TcM Bool
isUniqueTyName n = do
  uenv <- gets uniqueTypes
  pure $ any (\d -> dataName d == n) (Map.elems uenv)

isPartialDataType :: Name -> TcM Bool
isPartialDataType n =
  gets (Set.member n . partialDataTypes)

visibleConstructorsForPartialDataType :: Name -> TcM (Maybe (Set.Set Name))
visibleConstructorsForPartialDataType n =
  gets (Map.lookup n . partialDataTypeConstructors)

withPartialDataTypesDisabled :: TcM a -> TcM a
withPartialDataTypesDisabled action = do
  partialTypes <- gets partialDataTypes
  partialTypeConstructors <- gets partialDataTypeConstructors
  modify (\env -> env {partialDataTypes = Set.empty, partialDataTypeConstructors = Map.empty})
  action
    `catchError` ( \err -> do
                     modify (\env -> env {partialDataTypes = partialTypes, partialDataTypeConstructors = partialTypeConstructors})
                     throwError err
                 )
    >>= \result -> do
      modify (\env -> env {partialDataTypes = partialTypes, partialDataTypeConstructors = partialTypeConstructors})
      pure result

typeInfoFor :: DataTy -> TypeInfo
typeInfoFor (DataTy _ vs cons) =
  TypeInfo (length vs) (map constrName cons) []

freshTyVar :: TcM Ty
freshTyVar = Meta <$> freshVar

freshSkolem :: TcM Tyvar
freshSkolem = Skolem <$> freshName

writeFunDef :: FunDef Id -> TcM ()
writeFunDef fd = writeTopDecl (TFunDef fd)

writeDataTy :: DataTy -> TcM ()
writeDataTy dt =
  writeTopDecl (TDataDef dt)

writeInstance :: Instance Id -> TcM ()
writeInstance instd =
  writeTopDecl (TInstDef instd)

writeTopDecl :: TopDecl Id -> TcM ()
writeTopDecl d =
  do
    b <- gets generateDefs
    when b $ do
      ts <- gets generated
      modify (\env -> env {generated = d : ts})

getEnvFreeVars :: TcM [Tyvar]
getEnvFreeVars =
  concat <$> gets (Map.map fv . ctx)

getEnvFreeVarSet :: TcM (Set.Set Tyvar)
getEnvFreeVarSet = do
  tvMaps <- gets (Map.map fv . ctx)
  pure $ elemsSet tvMaps
  where
    elemsSet :: Map.Map Name [Tyvar] -> Set.Set Tyvar
    elemsSet = Map.foldr addElems (Set.empty :: Set.Set Tyvar)
    addElems :: [Tyvar] -> Set.Set Tyvar -> Set.Set Tyvar
    addElems vars set = foldr Set.insert set vars

getEnvMetaVars :: TcM [MetaTv]
getEnvMetaVars =
  concat <$> gets (Map.map mv . ctx)

unify :: Ty -> Ty -> TcM Subst
unify t t' =
  do
    s <- getSubst
    s' <- tcmMgu (apply s t) (apply s t')
    s1 <- extSubst s'
    pure s1

matchTy :: Ty -> Ty -> TcM Subst
matchTy t t' =
  do
    s <- tcmMatch t t'
    extSubst s

tcmMatch :: Ty -> Ty -> TcM Subst
tcmMatch = match

addFunctionName :: Name -> TcM ()
addFunctionName n =
  modify (\env -> env {directCalls = n : directCalls env})

isDirectCall :: Name -> TcM Bool
isDirectCall (QualName _ _) = pure True
isDirectCall n =
  (elem n) <$> gets directCalls

-- including contructors on environment

checkDataType :: DataTy -> TcM ()
checkDataType d@(DataTy n vs constrs) =
  do
    -- check if the type is already defined.
    r <- maybeAskTypeInfo n
    unless (isNothing r) $
      typeAlreadyDefinedError d n
    let vals' = map (\(name', ty) -> (name', Forall (bv ty) ([] :=> ty))) vals
    mapM_ (uncurry extEnv) vals'
    modifyTypeInfo n ti
    -- checking kinds
    mapM_ kindCheck (concatMap constrTy constrs) `wrapError` d
  where
    ti = TypeInfo (length vs) (map fst vals) []
    tc = TyCon n (TyVar <$> vs)
    vals = map constrBind constrs
    constrBind c = (constrName c, (funtype (constrTy c) tc))

-- kind check

kindCheck :: Ty -> TcM Ty
kindCheck (t1 :-> t2) =
  (:->) <$> kindCheck t1 <*> kindCheck t2
kindCheck t@(TyCon n ts) =
  do
    ti <- askTypeInfo n `wrapError` t
    unless (n == Name "pair" || arity ti == length ts) $
      tcmError $
        unlines
          [ "Invalid number of type arguments!",
            "Type "
              ++ pretty n
              ++ " is expected to have "
              ++ show (arity ti)
              ++ " type arguments",
            "but, type "
              ++ pretty t
              ++ " has "
              ++ (show $ length ts)
              ++ " arguments"
          ]
    ts' <- mapM kindCheck ts
    pure (TyCon n ts')
kindCheck t = pure t

-- Skolemization

skolemise :: Scheme -> TcM ([Tyvar], Qual Ty)
skolemise sch@(Forall _ qt) =
  do
    let bvs = bv sch
    sks <- mapM (const freshSkolem) bvs
    pure (sks, insts (zip bvs (map TyVar sks)) qt)

-- type instantiation

class Fresh a where
  type Result a
  freshInst :: a -> TcM (Result a)

instance Fresh Scheme where
  type Result Scheme = (Qual Ty)
  freshInst sch@(Forall _ qt) =
    do
      let vs = bv sch
          ns = map tyvarName vs
      ns' <- gets nameSupply
      modify (\env -> env {nameSupply = ns' \\ ns})
      mvs <- mapM (const freshTyVar) ns
      let qt' = insts (zip vs mvs) qt
      pure qt'

instance Fresh Inst where
  type Result Inst = Inst
  freshInst it@(_ :=> _) =
    do
      let vs = bv it
      mvs <- mapM (const freshTyVar) vs
      pure (insts (zip vs mvs) it)

fromANF :: Inst -> TcM Inst
fromANF (ps :=> p) =
  do
    let eqs = [(t, t') | (t :~: t') <- ps]
        ps' = [c | c@(InCls _ _ _) <- ps]
    s <- solve eqs mempty
    pure $ apply s (ps' :=> p)

instance Fresh ClassInfo where
  type Result ClassInfo = ClassInfo

  freshInst cls =
    do
      let vs = bv (classpred cls) `union` bv (supers cls)
      mvs <- mapM (const freshTyVar) vs
      let env = zip vs mvs
      pure $
        cls
          { classpred = insts env (classpred cls),
            supers = insts env (supers cls)
          }

type IEnv = [(Tyvar, Ty)]

class Insts a where
  insts :: IEnv -> a -> a

instance (Insts a) => Insts [a] where
  insts env = map (insts env)

instance Insts Ty where
  insts env (TyCon n ts) = TyCon n (insts env ts)
  insts env (TyVar v) = fromMaybe (TyVar v) (lookup v env)
  insts _ (Meta v) = Meta v

instance Insts Pred where
  insts env (InCls n t ts) =
    InCls n (insts env t) (insts env ts)
  insts env (t1 :~: t2) =
    (insts env t1) :~: (insts env t2)

instance (Insts a) => Insts (Qual a) where
  insts env (ps :=> p) =
    (insts env ps) :=> (insts env p)

-- substitution

withCurrentSubst :: (HasType a) => a -> TcM a
withCurrentSubst t = do
  s <- gets subst
  pure (apply s t)

getSubst :: TcM Subst
getSubst = gets subst

putSubst :: Subst -> TcM ()
putSubst s = modify (\env -> env {subst = s})

extSubst :: Subst -> TcM Subst
extSubst s = modify ext >> getSubst
  where
    ext st = st {subst = s <> subst st}

withLocalSubst :: (HasType a) => TcM a -> TcM a
withLocalSubst m =
  do
    s <- getSubst
    r <- m
    modify (\st -> st {subst = s})
    pure (apply s r)

clearSubst :: TcM ()
clearSubst = modify (\st -> st {subst = mempty})

-- current contract manipulation

withContractName :: Name -> TcM a -> TcM a
withContractName n m =
  do
    setCurrentContract n
    a <- m
    clearCurrentContract
    pure a

setCurrentContract :: Name -> TcM ()
setCurrentContract n =
  modify (\st -> st {contract = Just n})

clearCurrentContract :: TcM ()
clearCurrentContract =
  modify (\st -> st {contract = Nothing})

askCurrentContract :: TcM Name
askCurrentContract =
  do
    n <- gets contract
    maybe
      (tcmError "Impossible! Lacking current contract name!")
      pure
      n

-- manipulating contract field information

askField :: Name -> Name -> TcM Scheme
askField cn fn =
  do
    ti <- askTypeInfo cn
    when
      (fn `notElem` fieldNames ti)
      (undefinedField cn fn)
    askEnv fn

-- manipulating data constructor information

checkConstr :: Name -> Name -> TcM ()
checkConstr tn cn =
  do
    ti <- askTypeInfo tn
    unless
      (validConstr cn ti)
      (undefinedConstr tn cn)

validConstr :: Name -> TypeInfo -> Bool
validConstr n ti = n `elem` constrNames ti || isPair n
  where
    isPair (Name pairName) = pairName == "pair"
    isPair _ = False

-- extending the environment with a new variable

extEnv :: Name -> Scheme -> TcM ()
extEnv n t =
  modify (\sig -> sig {ctx = Map.insert n t (ctx sig)})

withExtEnv :: Name -> Scheme -> TcM a -> TcM a
withExtEnv n s m =
  withLocalEnv (extEnv n s >> m)

withLocalCtx :: [(Name, Scheme)] -> TcM a -> TcM a
withLocalCtx envPairs m =
  withLocalEnv do
    mapM_ (\(n, s) -> extEnv n s) envPairs
    a <- m
    pure a

-- Updating the environment

putEnv :: Env -> TcM ()
putEnv envCtx =
  modify (\sig -> sig {ctx = envCtx})

-- Extending the environment

withLocalEnv :: TcM a -> TcM a
withLocalEnv ta =
  do
    savedCtx <- gets ctx
    a <- ta
    putEnv savedCtx
    pure a

envList :: TcM [(Name, Scheme)]
envList = gets (Map.toList . ctx)

-- asking class info

askClassInfo :: Name -> TcM ClassInfo
askClassInfo n =
  do
    r <- Map.lookup n <$> gets classTable
    case r of
      Nothing -> undefinedClass n
      Just cinfo -> do
        let vs = bv (classpred cinfo : supers cinfo)
        fs <- mapM (const freshTyVar) vs
        let env = zip vs fs
        pure
          ( cinfo
              { classpred = insts env (classpred cinfo),
                supers = insts env (supers cinfo)
              }
          )

-- environment operations: variables

maybeAskEnv :: Name -> TcM (Maybe Scheme)
maybeAskEnv n = gets (Map.lookup n . ctx)

askEnv :: Name -> TcM Scheme
askEnv n =
  do
    s <- maybeAskEnv n
    maybe (undefinedName n) pure s

-- type information

maybeAskTypeInfo :: Name -> TcM (Maybe TypeInfo)
maybeAskTypeInfo n =
  gets (Map.lookup n . typeTable)

askTypeInfo :: Name -> TcM TypeInfo
askTypeInfo n =
  do
    ti <- maybeAskTypeInfo n
    maybe (undefinedType n) pure ti

modifyTypeInfo :: Name -> TypeInfo -> TcM ()
modifyTypeInfo n ti =
  do
    tenv <- gets typeTable
    let tenv' = Map.insert n ti tenv
    modify (\env -> env {typeTable = tenv'})

-- type synonym information

maybeAskSynInfo :: Name -> TcM (Maybe SynInfo)
maybeAskSynInfo n =
  gets (Map.lookup n . synTable)

askSynInfo :: Name -> TcM SynInfo
askSynInfo n =
  do
    si <- maybeAskSynInfo n
    maybe (undefinedSynonym n) pure si

addSynInfo :: Name -> SynInfo -> TcM ()
addSynInfo n si =
  modify (\env -> env {synTable = Map.insert n si (synTable env)})

isSynonym :: Name -> TcM Bool
isSynonym n = isJust <$> maybeAskSynInfo n

checkSynonym :: TySym -> TcM ()
checkSynonym (TySym n vs t) =
  do
    r <- maybeAskSynInfo n
    unless (isNothing r) $
      duplicatedSynonymDecl n
    let si = SynInfo (length vs) vs t
    addSynInfo n si

duplicatedSynonymDecl :: Name -> TcM a
duplicatedSynonymDecl n =
  tcDiagnosticErrorAtName
    "SC0226"
    ("duplicate type synonym definition: " ++ pretty n)
    n
    "duplicate type synonym"
    []
    ["rename or remove the duplicate type synonym"]

-- manipulating the instance environment

getClassEnv :: TcM ClassTable
getClassEnv =
  gets classTable >>= renameClassEnv

askInstEnv :: Name -> TcM [Inst]
askInstEnv n =
  maybe [] id . Map.lookup n <$> getInstEnv

getInstEnv :: TcM InstTable
getInstEnv =
  gets instEnv >>= renameInstEnv

getDefaultInstEnv :: TcM InstTable
getDefaultInstEnv =
  do
    denv <- Map.map (\i -> [i]) <$> gets defaultEnv
    renameInstEnv denv

renameClassEnv :: ClassTable -> TcM ClassTable
renameClassEnv =
  traverse go
  where
    go v = freshInst v

renameInstEnv :: InstTable -> TcM InstTable
renameInstEnv =
  Map.traverseWithKey go
  where
    go _ v = mapM freshInst v

addInstance :: Name -> Inst -> TcM ()
addInstance n inst =
  do
    modify
      ( \st ->
          st {instEnv = Map.insertWith (++) n [inst] (instEnv st)}
      )

addDefaultInstance :: Name -> Inst -> TcM ()
addDefaultInstance n inst =
  do
    modify
      ( \st ->
          st {defaultEnv = Map.insert n inst (defaultEnv st)}
      )

maybeToTcM :: String -> Maybe a -> TcM a
maybeToTcM s Nothing = tcmError s
maybeToTcM _ (Just x) = pure x

-- checking coverage pragma

pragmaEnabled :: Name -> PragmaStatus -> Bool
pragmaEnabled _ Enabled = False
pragmaEnabled _ DisableAll = True
pragmaEnabled n (DisableFor ns) = n `elem` N.toList ns

askCoverage :: Name -> TcM Bool
askCoverage n =
  (pragmaEnabled n) <$> gets coverage

setCoverage :: PragmaStatus -> TcM ()
setCoverage st =
  modify (\env -> env {coverage = st})

-- checking Patterson condition pragma

askPattersonCondition :: Name -> TcM Bool
askPattersonCondition n =
  (pragmaEnabled n) <$> gets patterson

setPattersonCondition :: PragmaStatus -> TcM ()
setPattersonCondition st =
  modify (\env -> env {patterson = st})

-- checking bound variable condition

askBoundVariableCondition :: Name -> TcM Bool
askBoundVariableCondition n =
  (pragmaEnabled n) <$> gets boundVariable

setBoundVariableCondition :: PragmaStatus -> TcM ()
setBoundVariableCondition st =
  modify (\env -> env {boundVariable = st})

isTrustedImportedInstance :: Instance Name -> TcM Bool
isTrustedImportedInstance inst =
  (instanceHeadKey inst `elem`) <$> gets trustedInstanceHeads

disableBoundVariableCondition :: TcM a -> TcM a
disableBoundVariableCondition m =
  do
    old <- gets boundVariable
    setBoundVariableCondition DisableAll
    x <- m
    setBoundVariableCondition old
    pure x

-- recursion depth

askMaxRecursionDepth :: TcM Int
askMaxRecursionDepth = gets maxRecursionDepth

-- other options

getTcOpts :: TcM Option
getTcOpts = gets tcOptions

getNoDesugarCalls :: TcM Bool
getNoDesugarCalls = gets (optNoDesugarCalls . tcOptions)

-- logging utilities

setLogging :: Bool -> TcM ()
setLogging b = modify (\r -> r {enableLog = b})

isLogging :: TcM Bool
isLogging = gets enableLog

isVerbose :: TcM Bool
isVerbose = gets (optVerbose . tcOptions)

info :: [String] -> TcM ()
info ss = do
  let msg = concat ss
  logging <- isLogging
  verbose <- isVerbose
  when logging $ modify (\r -> r {logs = msg : logs r})
  when verbose $ liftIO $ putStrLn msg

infoDoc :: Doc -> TcM ()
infoDoc d = info [render d]

infoDocs :: [Doc] -> TcM ()
infoDocs = infoDoc . sep

warning :: String -> TcM ()
warning s = do
  modify (\r -> r {warnings = s : "Warning:" : warnings r})

dumpLogs :: TcM ()
dumpLogs = do
  records <- gets logs
  liftIO $ putStrLn "\nLogs:"
  liftIO $ putStrLn $ unlines $ reverse records
  liftIO $ putStrLn "------------------------------------------------------------------"

-- wrapping error messages

wrapError :: (Pretty b, Data b) => TcM a -> b -> TcM a
wrapError m e =
  catchError m handler
  where
    handler msg = throwError (decorate msg)
    decorate (CompilerDiagnostics diagnostics) =
      CompilerDiagnostics $
        map
          (addDiagnosticNote ("in: " ++ pretty e) . addContextLabel e)
          diagnostics
    decorate (CompilerLegacyError msg) =
      CompilerLegacyError (msg ++ "\n - in:" ++ pretty e)

addContextLabel :: (Data b) => b -> Diagnostic -> Diagnostic
addContextLabel context diagnostic
  | any ((== Primary) . labelStyle) (diagnosticLabels diagnostic) = diagnostic
  | otherwise =
      case contextSourceSpan context of
        Just sourceSpan ->
          diagnostic
            { diagnosticLabels =
                Label
                  { labelSpan = sourceSpan,
                    labelStyle = Primary,
                    labelMessage = Just (contextLabelMessage diagnostic)
                  }
                  : diagnosticLabels diagnostic
            }
        Nothing -> diagnostic

contextLabelMessage :: Diagnostic -> String
contextLabelMessage diagnostic =
  case diagnosticCode diagnostic of
    Just (DiagnosticCode "SC0201") -> "expression has mismatched type"
    Just (DiagnosticCode "SC0202") -> "unknown name"
    Just (DiagnosticCode "SC0203") -> "undefined type"
    Just (DiagnosticCode "SC0204") -> "undefined field"
    Just (DiagnosticCode "SC0205") -> "undefined constructor"
    Just (DiagnosticCode "SC0206") -> "undefined function"
    Just (DiagnosticCode "SC0207") -> "undefined class"
    Just (DiagnosticCode "SC0208") -> "undefined type synonym"
    Just (DiagnosticCode "SC0209") -> "type is not polymorphic enough"
    Just (DiagnosticCode "SC0220") -> "incomplete signature"
    Just (DiagnosticCode "SC0221") -> "incomplete method signature"
    Just (DiagnosticCode "SC0222") -> "return before end of block"
    Just (DiagnosticCode "SC0223") -> "unsolved constraint"
    Just (DiagnosticCode "SC0224") -> "shorthand constructor"
    Just (DiagnosticCode "SC0225") -> "duplicate function"
    Just (DiagnosticCode "SC0226") -> "duplicate type synonym"
    Just (DiagnosticCode "SC0227") -> "duplicate class"
    Just (DiagnosticCode "SC0228") -> "duplicate class method"
    Just (DiagnosticCode "SC0229") -> "duplicate type"
    _ -> "diagnostic reported here"

contextSourceSpan :: (Data a) => a -> Maybe SourceSpan
contextSourceSpan value =
  getFirst $ everything (<>) (mkQ (First Nothing) locationSpan `extQ` nameSpan) value
  where
    locationSpan :: NodeLocation -> First SourceSpan
    locationSpan = First . nodeLocationSpan

    nameSpan :: Name -> First SourceSpan
    nameSpan = First . nameSourceSpan

tcmMgu :: Ty -> Ty -> TcM Subst
tcmMgu = mgu

-- error messages

tcmError :: String -> TcM a
tcmError s = do
  verbose <- isVerbose
  when verbose dumpLogs
  throwError (genericTypecheckError s)

genericTypecheckError :: String -> CompilerError
genericTypecheckError rawMessage =
  diagnosticCompilerError $
    Diagnostic
      { diagnosticSeverity = Error,
        diagnosticCode = Just (DiagnosticCode "SC0299"),
        diagnosticMessage = message,
        diagnosticLabels = [],
        diagnosticNotes = notes,
        diagnosticHelp = []
      }
  where
    rawLines = dropWhile null (lines rawMessage)
    (message, notes) =
      case rawLines of
        [] -> ("typecheck error", [])
        firstLine : rest -> (firstLine, filter (not . null) rest)

undefinedName :: Name -> TcM a
undefinedName n =
  tcDiagnosticErrorAtName
    "SC0202"
    ("undefined name: " ++ pretty n)
    n
    "unknown name"
    []
    []

undefinedType :: Name -> TcM a
undefinedType n =
  do
    s <- (unlines . reverse) <$> gets logs
    tcDiagnosticErrorAtName
      "SC0203"
      ("undefined type: " ++ pretty n)
      n
      "undefined type"
      (if null s then [] else [s])
      []

undefinedField :: Name -> Name -> TcM a
undefinedField n n' =
  tcDiagnosticErrorAtName
    "SC0204"
    ("undefined field: " ++ pretty n)
    n
    "undefined field"
    ["in type: " ++ pretty n']
    []

undefinedConstr :: Name -> Name -> TcM a
undefinedConstr tn cn =
  tcDiagnosticErrorAtName
    "SC0205"
    ("undefined constructor: " ++ pretty cn)
    cn
    "undefined constructor"
    ["in type: " ++ pretty tn]
    []

undefinedFunction :: Name -> Name -> TcM a
undefinedFunction t n =
  tcDiagnosticErrorAtName
    "SC0206"
    ("undefined function: " ++ pretty n)
    n
    "undefined function"
    ["type " ++ pretty t ++ " does not define this function"]
    []

typeNotPolymorphicEnough :: Signature Name -> Scheme -> Scheme -> TcM a
typeNotPolymorphicEnough sig sch1 sch2 =
  tcDiagnosticErrorAtName
    "SC0209"
    "type is not polymorphic enough"
    (sigName sig)
    "annotated type is not polymorphic enough"
    [ "annotated type: " ++ pretty sch2,
      "inferred type: " ++ pretty sch1,
      "in: " ++ pretty sig
    ]
    []

undefinedClass :: Name -> TcM a
undefinedClass n =
  tcDiagnosticErrorAtName
    "SC0207"
    ("undefined class: " ++ pretty n)
    n
    "undefined class"
    []
    []

undefinedSynonym :: Name -> TcM a
undefinedSynonym n =
  tcDiagnosticErrorAtName
    "SC0208"
    ("undefined type synonym: " ++ pretty n)
    n
    "undefined type synonym"
    []
    []

tcDiagnosticError :: String -> String -> [String] -> [String] -> TcM a
tcDiagnosticError code message notes help =
  tcDiagnosticErrorWithLabels code message [] notes help

tcDiagnosticErrorAtName :: String -> String -> Name -> String -> [String] -> [String] -> TcM a
tcDiagnosticErrorAtName code message identName label notes help =
  tcDiagnosticErrorWithLabels code message (maybe [] pure (primaryNameLabel label identName)) notes help

tcDiagnosticErrorAtSource :: (HasSourceSpan source) => String -> String -> source -> String -> [String] -> [String] -> TcM a
tcDiagnosticErrorAtSource code message source label notes help =
  tcDiagnosticErrorWithLabels code message (maybe [] pure (primarySourceLabel label source)) notes help

tcDiagnosticErrorWithLabels :: String -> String -> [Label] -> [String] -> [String] -> TcM a
tcDiagnosticErrorWithLabels code message labels notes help =
  throwError $
    diagnosticCompilerError $
      Diagnostic
        { diagnosticSeverity = Error,
          diagnosticCode = Just (DiagnosticCode code),
          diagnosticMessage = message,
          diagnosticLabels = labels,
          diagnosticNotes = notes,
          diagnosticHelp = help
        }

primaryNameLabel :: String -> Name -> Maybe Label
primaryNameLabel message identName =
  do
    sourceSpan <- nameSourceSpan identName
    pure
      Label
        { labelSpan = sourceSpan,
          labelStyle = Primary,
          labelMessage = Just message
        }

primarySourceLabel :: (HasSourceSpan source) => String -> source -> Maybe Label
primarySourceLabel message source =
  do
    sourceSpan <- sourceSpanOf source
    pure
      Label
        { labelSpan = sourceSpan,
          labelStyle = Primary,
          labelMessage = Just message
        }

topLevelFunctionAnnotationError :: Signature Name -> TcM a
topLevelFunctionAnnotationError sig =
  tcDiagnosticErrorAtName
    "SC0220"
    "top-level function must have complete type annotations"
    (sigName sig)
    "incomplete signature"
    ["signature: " ++ pretty sig]
    ["annotate every parameter (name : Type) and provide a return type (-> Type)"]

methodAnnotationError :: Signature Name -> TcM a
methodAnnotationError sig =
  tcDiagnosticErrorAtName
    "SC0221"
    "class and instance methods must have complete type signatures"
    (sigName sig)
    "incomplete method signature"
    ["signature: " ++ pretty sig]
    ["annotate every method parameter and provide a return type"]

illegalReturnStatement :: Stmt Name -> TcM a
illegalReturnStatement stmt =
  tcDiagnosticErrorAtSource
    "SC0222"
    "illegal return statement"
    stmt
    "return before end of block"
    ["return statements must be the final statement in a block"]
    []

cannotEntail :: Pred -> [String] -> TcM a
cannotEntail predValue notes =
  tcDiagnosticError
    "SC0223"
    ("cannot entail: " ++ pretty predValue)
    notes
    ["add a matching instance or strengthen the surrounding type context"]

shorthandConstructorError :: String -> Name -> [String] -> TcM a
shorthandConstructorError message constructorName notes =
  tcDiagnosticErrorAtName
    "SC0224"
    message
    constructorName
    "shorthand constructor"
    notes
    ["use a constructor that is visible for the expected type"]

typeAlreadyDefinedError :: DataTy -> Name -> TcM a
typeAlreadyDefinedError d n =
  do
    -- get type info
    di <- askTypeInfo n
    d' <- dataTyFromInfo n di `wrapError` d
    tcDiagnosticErrorAtName
      "SC0229"
      ("duplicate type definition: " ++ pretty n)
      n
      "duplicate type"
      ["new definition: " ++ pretty d, "existing definition: " ++ pretty d']
      ["rename or remove the duplicate type definition"]

dataTyFromInfo :: Name -> TypeInfo -> TcM DataTy
dataTyFromInfo n (TypeInfo _ cs _) =
  do
    -- getting data constructor types
    (constrs, vs) <- unzip <$> mapM constrsFromEnv cs
    pure (DataTy n (concat vs) constrs)

constrsFromEnv :: Name -> TcM (Constr, [Tyvar])
constrsFromEnv n =
  do
    (Forall vs (_ :=> ty)) <- askEnv n
    let (ts, _) = splitTy ty
    pure (Constr n ts, vs)

writeln :: String -> TcM ()
writeln = liftIO . putStrLn

timeItNamed :: String -> TcM a -> TcM a
timeItNamed n m = do
  (time, a) <- TimeIt.timeItT m
  opts <- gets tcOptions
  when (optTiming opts && time > 0.1) $ writeln (printf "%s: %.2f" n time)
  return a
