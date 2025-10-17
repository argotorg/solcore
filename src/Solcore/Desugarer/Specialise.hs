-- {-# LANGUAGE DefaultSignatures #-}
module Solcore.Desugarer.Specialise where  --(specialiseCompUnit, typeOfTcExp) where
{- * Specialisation
Create specialised versions of polymorphic and overloaded functions.
This is meant to be run on typed and defunctionalised code, so no higher-order functions.
-}

import Common.Monad
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics
import Data.List(intercalate, union, (\\))
import Data.Maybe(fromMaybe)
import qualified Data.Map as Map
import GHC.Stack
import Solcore.Desugarer.IfDesugarer(desugaredBoolTy)
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id ( Id(..) )
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcEnv(TcEnv(..),TypeInfo(..))
import qualified Solcore.Frontend.TypeInference.TcSubst as TcSubst
import Solcore.Frontend.TypeInference.TcUnify(typesDoNotUnify)
import Solcore.Frontend.Pretty.ShortName
import Solcore.Primitives.Primitives
import System.Exit
import Common.Pretty

-- ** Specialisation state and monad
-- SpecState and SM are meant to be local to this module.
type Table a = Map.Map Name a
emptyTable :: Table a
emptyTable = Map.empty

type TcFunDef = FunDef Id
type TcExp = Exp Id

type Resolution = (Ty, TcFunDef)
data SpecState = SpecState
  { spResTable :: Table [Resolution]
  , specTable :: Table TcFunDef
  , spTypeTable :: Table TypeInfo
  , spDataTable :: Table DataTy
  , spGlobalEnv :: TcEnv
  , splocalEnv :: Table Ty
  , spSubst :: TVSubst
  , spDebug :: Bool
  , spNS :: NameSupply
  }


type SM = StateT SpecState IO

getDebug :: SM Bool
getDebug = gets spDebug

withDebug m = do
    savedDebug <- getDebug
    modify $ \s -> s { spDebug = True }
    a <- m
    modify $ \s -> s { spDebug = savedDebug }
    return a

whenDebug m = do
    debug <- getDebug
    when debug m

debug :: [String] -> SM ()
debug msg = do
    enabled <- getDebug
    when enabled $ writes msg

runSM :: Bool -> TcEnv -> SM a -> IO a
runSM debugp env m = evalStateT m (initSpecState debugp env)

-- prettys :: Pretty a => [a] -> String
-- prettys = render . brackets . commaSep . map ppr

-- | `withLocalState` runs a computation with a local state
-- local changes are discarded, with the exception of the `specTable` and name supply
withLocalState :: SM a -> SM a
withLocalState m = do
    s <- get
    a <- m
    spTable <- gets specTable
    ns <- gets spNS
    put s
    modify $ \s -> s { specTable = spTable, spNS = ns }
    return a

initSpecState :: Bool ->TcEnv -> SpecState
initSpecState debugp env = SpecState
    { spResTable = emptyTable
    , specTable = emptyTable
    , spTypeTable = typeTable env
    , spDataTable = Map.empty
    , spGlobalEnv = env
    , splocalEnv = emptyTable
    , spSubst = emptyTVSubst
    , spDebug = debugp
    , spNS = namePool
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

-- | A signature forall tvs . t is considered ambiguous if `tvs \\ FTV(t) /= mempty`
-- this is should be the same as `FTV(body) \\ FTV(t) /= {}`
-- returns list of ambiguous variables
ambiguousVarsInSig :: HasTV a => Signature a -> [Tyvar]
ambiguousVarsInSig sig = sigVars sig \\ freetv (sigParams sig, sigReturn sig)

addSpecialisation :: Name -> TcFunDef -> SM ()
addSpecialisation name fd = modify $ \s -> s { specTable = Map.insert name fd (specTable s) }

lookupSpecialisation :: Name -> SM (Maybe TcFunDef)
lookupSpecialisation name = gets (Map.lookup name . specTable)

addResolution :: Name -> Ty -> TcFunDef -> SM ()
addResolution name ty fun = do
    -- debug ["+ addResolution ", pretty name, "@", pretty ty, " |-> ", shortName fun]
    let sig = funSignature fun
    reportAmbiguousVars sig
    modify $ \s -> s { spResTable = Map.insertWith (++) name [(ty, fun)] (spResTable s) }
    where
      reportAmbiguousVars sig = do
        let vars = ambiguousVarsInSig sig
        let scheme = schemeOfTcSignature sig
        unless (null vars) $ nopanics [ "Error: function ", pretty name
                        ," cannot be specialised because it has an ambiguous type:\n   "
                        , pretty scheme
                        ,"\n variables: ", prettys vars
                        ,"\n do not occur in the argument/result types."
                        ]
lookupResolution :: Name -> Ty ->  SM (Maybe (TcFunDef, Ty, TVSubst))
lookupResolution name ty = gets (Map.lookup name . spResTable) >>= findMatch ty where
  str :: Pretty a => a -> String
  str = pretty
  findMatch :: Ty -> Maybe [Resolution] -> SM (Maybe (TcFunDef, Ty, TVSubst))
  findMatch etyp (Just res) = do
    debug ["|> findMatch ", pretty etyp, " in ", prettys res]
    firstMatch etyp res
  findMatch _ Nothing = return Nothing
  firstMatch :: Ty -> [Resolution] -> SM (Maybe (TcFunDef, Ty, TVSubst))
  firstMatch etyp [] = return Nothing
  firstMatch etyp ((t,e):rest)
    | Right subst <- specmgu t etyp = do  -- TESTME: match is to weak for MPTC, but isn't mgu too strong?
        debug ["< lookupRes - match found for ", str name, ": ", str t, " ~ ", str etyp, " => ", str subst]
        return (Just (e, t, subst))
    | otherwise = firstMatch etyp rest

getSpSubst :: SM TVSubst
getSpSubst = gets spSubst

putSpSubst :: TVSubst -> SM ()
putSpSubst subst = modify $ \s -> s { spSubst = subst }
extSpSubst :: TVSubst -> SM ()

extSpSubst subst = modify $ \s -> s { spSubst = spSubst s <> subst }

atCurrentSubst :: HasTV a => a -> SM a
atCurrentSubst a = flip applytv a <$> getSpSubst

addData :: DataTy -> SM ()
addData dt = modify (\s -> s { spDataTable = Map.insert (dataName dt) dt (spDataTable s) })

spNewName :: SM Name
spNewName = do
    s <- get
    let (n, ns) = newName (spNS s)
    put s { spNS = ns }
    pure (addPrefix "_" n)

-- data Name = Name String | QualName Name String
addPrefix :: String -> Name -> Name
addPrefix p (Name s) = Name (p ++ s)
addPrefix p (QualName q s) = QualName q (p ++ s)

-------------------------------------------------------------------------------

specialiseCompUnit :: CompUnit Id -> Bool -> TcEnv -> IO (CompUnit Id)
specialiseCompUnit compUnit debugp env = runSM debugp env do
    addGlobalResolutions compUnit
    topDecls <- concat <$> forM (contracts compUnit) specialiseTopDecl
    return $ compUnit { contracts = topDecls }

addGlobalResolutions :: CompUnit Id -> SM ()
addGlobalResolutions compUnit = forM_ (contracts compUnit) addDeclResolutions

addDeclResolutions :: TopDecl Id -> SM ()
addDeclResolutions (TInstDef inst) = addInstResolutions inst
addDeclResolutions (TFunDef fd) = addFunDefResolution fd
addDeclResolutions (TDataDef dt) = addData dt
addDeclResolutions (TMutualDef decls) = forM_ decls addDeclResolutions
addDeclResolutions _ = return ()


addInstResolutions :: Instance Id -> SM ()
addInstResolutions inst = forM_ (instFunctions inst) (addMethodResolution (instName inst) (mainTy inst))

specialiseTopDecl :: TopDecl Id -> SM [TopDecl Id]
specialiseTopDecl (TContr (Contract name args decls)) = withLocalState do
    addContractResolutions (Contract name args decls)
    -- Runtime code
    runtimeDecls <- withLocalState do
       forM_ entries specEntry
       getSpecialisedDecls
    -- Deployer code
    modify (\st -> st { specTable = emptyTable })
    let deployerName = Name (pretty name <> "$Deployer")
    let mconstructor = findConstructor decls
    deployDecls <- case mconstructor of
      Just c -> withLocalState do
        cname' <- specConstructor c
        st <- gets specTable
        let cdecl = st Map.! cname'
        depDecls <- getSpecialisedDecls
        -- use mutual to group constructor with its dependencies
        pure [CMutualDecl depDecls]
      Nothing -> pure []
    return [TContr (Contract name args (deployDecls ++ runtimeDecls))]
    where
      entries = ["main"]    -- Eventually all public methods
      getSpecialisedDecls :: SM [ContractDecl Id]
      getSpecialisedDecls = do
        st <- gets specTable
        dt <- gets spDataTable
        let dataDecls = map (CDataDecl . snd) (Map.toList dt)
        let funDecls = map (CFunDecl . snd) (Map.toList st)
        pure (dataDecls ++ funDecls)

-- keep datatype defs intact
specialiseTopDecl d@TDataDef{} = pure [d]
-- Drop all toplevel decls that are not contracts - we do not need them anymore
specialiseTopDecl decl = pure []

findConstructor :: [ContractDecl Id] -> Maybe (Constructor Id)
findConstructor = foldr (\d -> (getConstructor d <|>)) Nothing

-- findConstructor (c:cs) = getConstructor c <|> findConstructor cs

getConstructor :: ContractDecl Id -> Maybe (Constructor Id)
getConstructor (CConstrDecl c) = Just c
getConstructor _ = Nothing


specEntry :: Name -> SM ()
specEntry name = withLocalState do
    let any = TVar (Name "any")
    let anytype = TyVar any
    mres <- lookupResolution name anytype
    case mres of
      Just (fd, ty, subst) -> do
        debug ["< resolution: ", show name, " : ", pretty ty, "@", pretty subst]
        void(specFunDef fd)
      Nothing -> do
        warns ["!! Warning: no resolution found for ", show name]

specConstructor (Constructor [] body) = do
  let sig = Signature [] [] (Name "constructor") [] (Just unit)
  let fd = FunDef sig body
  specFunDef fd
specConstructor (Constructor params body) = error "Unsupported constructor"

addContractResolutions :: Contract Id -> SM ()
addContractResolutions (Contract name args decls) = do
  forM_ decls addCDeclResolution

addCDeclResolution :: ContractDecl Id -> SM ()
addCDeclResolution (CFunDecl fd) = addFunDefResolution fd
addCDeclResolution (CDataDecl dt) = addData dt
addCDeclResolution (CMutualDecl decls) = forM_ decls addCDeclResolution
addCDeclResolution _ = return ()

addFunDefResolution fd = do
  let sig = funSignature fd
  let name = sigName sig
  let funType = typeOfTcFunDef fd
  addResolution name funType fd
  debug ["+ addDeclResolution: ", show name, " : ", pretty funType]

addMethodResolution :: Name -> Ty -> TcFunDef -> SM ()
addMethodResolution cname ty fd = do
  let sig = funSignature fd
  let name = sigName sig
  let qname = case name of
        QualName{} -> name
        Name s -> QualName cname s
  let name' = specName qname [ty]
  let funType = typeOfTcFunDef fd
  let fd' = FunDef sig{sigName = name'} (funDefBody fd)
  addResolution qname funType fd'
  debug ["+ addMethodResolution: ", show qname, " / ", show name', " : ", pretty funType]

-- | `specExp` specialises an expression to given type
specExp :: TcExp -> Ty -> SM TcExp
specExp e@(Call Nothing i args) ty = do
  -- debug ["> specExp (Call): ", pretty e, " : ", pretty (idType i), " ~> ", pretty ty]
  (i', args') <- specCall i args ty
  let e' = Call Nothing i' args'
  -- debug ["< specExp (Call): ", pretty e']
  return e'
specExp e@(Con i@(Id n conty) es) ty = do
  let t = typeOfTcExp e
  -- debug ["> specConApp: ", pretty e, " : ", pretty t, " ~> ", pretty ty]
  (i' , es') <- specConApp i es ty
  let e' = Con i' es'
  return e'
specExp e@(Cond e1 e2 e3) ty = do
  e1' <- specExp e1 desugaredBoolTy
  e2' <- specExp e2 ty
  e3' <- specExp e3 ty
  pure (Cond e1' e2' e3')
specExp e@(Var (Id n t)) ty = pure (Var (Id n ty))
specExp e@(FieldAccess me fld) ty = error("Specialise: FieldAccess not implemented for" ++ pretty e)
specExp e@(TyExp e1 _) ty = specExp e1 ty
specExp e ty = atCurrentSubst e -- FIXME

specConApp :: Id -> [TcExp] -> Ty -> SM (Id, [TcExp])
-- specConApp i@(Id n conTy) [] ty = pure (i, [])
specConApp i@(Id n conTy) args ty = do
  subst <- getSpSubst
  let argTypes = map typeOfTcExp args
  let argTypes' = applytv subst argTypes
  let i' = applytv subst i
  let typedArgs = zip args argTypes'
  args' <- forM typedArgs (uncurry specExp)
  let conTy' = foldr (:->) ty argTypes'
  debug ["> specConApp: ", prettyId i, " : ", pretty conTy, " ~> ", prettyId i', " : ", pretty conTy']
  debug ["< specConApp: ", prettyConApp i args,  " ~> ", prettyConApp i' args']
  return (i', args')

-- | Specialise a function call
-- given actual arguments and the expected result type
specCall :: Id -> [TcExp] -> Ty -> SM (Id, [TcExp])
specCall i@(Id (Name "revert") e) args ty = pure (i, args)  -- FIXME
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
    Just (fd, ty, phi) -> do
      debug ["< resolution: ", show name, "~>", shortName fd, " : ", pretty ty, "@", pretty phi]
      extSpSubst phi
      -- ty' <- atCurrentSubst ty
      subst <- getSpSubst
      let ty' = applytv subst ty
      ensureClosed ty' (Call Nothing i args) subst
      name' <- specFunDef fd
      debug ["< specCall: ", pretty name']
      args'' <- atCurrentSubst args'
      return (Id name' ty', args'')
    Nothing -> do
      panics ["! specCall: no resolution found for ", show name, " : ", pretty funType]
      return (i, args')
  where
    guardSimpleType :: Ty -> SM ()
    guardSimpleType (Meta _) = panics ["specCall ", pretty i, ": polymorphic result type"]
    guardSimpleType (TyVar _) = panics ["specCall ", pretty i, ": polymorphic result type"]
    guardSimpleType (_ :-> _) = panics ["specCall ", pretty i, ": function result type"]
    guardSimpleType _ = pure ()

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
  let sig0 = funSignature fd
  let sig = funSignature fd
  let name = sigName sig
  let funType = typeOfTcFunDef fd
  let tvs = freetv funType
  subst <- renameSubst renaming <$> getSpSubst
  putSpSubst subst
  let tvs' = applytv subst (map TyVar tvs)
  debug ["> specFunDef ", pretty name, " : ", pretty funType,  " tvs'=", prettys tvs', " subst=", pretty subst]
  let name' = specName name tvs'
  let ty' = applytv subst funType
  mspec <- lookupSpecialisation name'
  case mspec of
    Just fd' -> return name'
    Nothing -> do
      let sig' = applytv subst (funSignature fd)
      -- add a placeholder first to break loops
      let placeholder = FunDef sig' []
      addSpecialisation name' placeholder
      body' <- specBody (funDefBody fd)
      let fd' = FunDef sig'{sigName = name'} body'
      debug ["+ specFunDef: adding specialisation ", show name', " : ", pretty ty']
      addSpecialisation name' fd'
      return name'

specBody :: [Stmt Id] -> SM [Stmt Id]
specBody = mapM specStmt

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
ensureClosed :: Pretty a => Ty -> a -> TVSubst ->  SM ()
ensureClosed ty ctxt subst = do
  let tvs = freetv ty
  unless (null tvs) $ panics ["spec(", pretty ctxt,"): free type vars in ", pretty ty, ": ", show tvs
                             , " @ subst=", pretty subst]
{-
  let mvs = mv ty
  unless (null tvs) $ panics ["spec(", pretty ctxt,"): free meta vars in ", pretty ty, ": ", show mvs
                             , " @ subst=", pretty subst]
-}

specStmt :: Stmt Id -> SM(Stmt Id)
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
  debug ["> specStmt (:=): ", pretty i, " : ", pretty (idType i)
        , " @ ", pretty subst, "~>'", pretty ty']
  ensureClosed ty' stmt subst
  e' <- specExp e ty'
  debug ["< specExp (:=): ", pretty e']
  return $ Var i' := e'

specStmt stmt@(Let i mty mexp) = do
  subst <- getSpSubst
  debug ["> specStmt (Let): ", pretty i, " : ", pretty (idType i), " @ ", pretty subst]
  i' <- atCurrentSubst i
  let ty' = idType i'
  ensureClosed ty' stmt subst
  mty' <- atCurrentSubst mty
  case mexp of
    Nothing -> return $ Let i' mty' Nothing
    Just e -> Let i' mty' . Just <$> specExp e ty'

specStmt (StmtExp e) = do
  ty <- atCurrentSubst (typeOfTcExp e)
  e' <- specExp e ty
  return $ StmtExp e'

specStmt (Asm ys) = pure (Asm ys)
specStmt stmt = errors ["specStmt not implemented for: ", show stmt]

specMatch :: [Exp Id] -> [([Pat Id], [Stmt Id])] -> SM (Stmt Id)
specMatch exps alts = do
  subst <- getSpSubst
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
      pat' <- atCurrentSubst pat
      return (pat', body')
    specScruts = mapM specScrut
    specScrut e = do
      subst <- getSpSubst
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
mangleTy (TyCon (Name n) ts) = n ++ "L" ++ intercalate "_" (map mangleTy ts) ++"J"

showId :: Id -> String
showId i =  showsId i ""
showsId (Id n t) = shows n .  ('@':) . showsPrec 10 t

prettyId :: Id -> String
prettyId = render . pprId

pprId :: Id -> Doc
pprId (Id n t@TyVar{}) = ppr n <> text "@" <> ppr t
pprId (Id n t@(TyCon cn [])) = ppr n <> "@" <> ppr t
pprId (Id n t) = ppr n <> text "@" <> parens(ppr t)

pprConApp :: Id -> [TcExp] -> Doc
pprConApp i args = pprId i <> brackets (commaSepList args)

prettyConApp :: Id -> [TcExp] -> String
prettyConApp i args = render (pprConApp i args)


typeOfTcExp :: TcExp -> Ty
typeOfTcExp (Var i)               = idType i
typeOfTcExp (Con i [])            = idType i
typeOfTcExp e@(Con i args)          = go (idType i) args where
  go ty [] = ty
  go (_ :-> u) (a:as) = go u as
  go _ _ = error $ "typeOfTcExp: " ++ show e
typeOfTcExp (Lit (IntLit _))      = word --TyCon "Word" []
typeOfTcExp exp@(Call Nothing i args) = applyTo args funTy where
  funTy = idType i
  applyTo [] ty = ty
  applyTo (_:as) (_ :-> u) = applyTo as u
  applyTo _ _ = error $ concat [ "apply ", pretty i, " : ", pretty funTy
                       , "to", show $ map pretty args
                       , "\nIn:\n", show exp
                       ]
typeOfTcExp (Lam args body (Just tb))       = funtype tas tb where
  tas = map typeOfTcParam args
typeOfTcExp (Cond _ _ e) = typeOfTcExp e
typeOfTcExp (TyExp _ ty) = ty
typeOfTcExp e = error $ "typeOfTcExp: " ++ show e

typeOfTcStmt :: Stmt Id -> Ty
typeOfTcStmt (n := e) = unit
typeOfTcStmt (Let n _ _) = idType n
typeOfTcStmt (StmtExp e) = typeOfTcExp e
typeOfTcStmt (Return e) = typeOfTcExp e
typeOfTcStmt (Match _ ((pat, body):_)) = typeOfTcBody body

typeOfTcBody :: [Stmt Id] -> Ty
typeOfTcBody []    = unit
typeOfTcBody [s]   = typeOfTcStmt s
typeOfTcBody (_:b) = typeOfTcBody b

typeOfTcParam :: Param Id -> Ty
typeOfTcParam (Typed i t)  = idType i  -- seems better than t - see issue #6
typeOfTcParam (Untyped i) = idType i

typeOfTcSignature :: Signature Id -> Ty
typeOfTcSignature sig = funtype (map typeOfTcParam $ sigParams sig) (returnType sig) where
  returnType sig = case sigReturn sig of
    Just t -> t
    Nothing -> error ("no return type in signature of: " ++ show (sigName sig))

schemeOfTcSignature :: Signature Id -> Scheme
schemeOfTcSignature sig@(Signature vs ps n args (Just rt))
  = if all isTyped args
      then Forall vs (ps :=> (funtype ts rt))
      else error $ unwords ["Invalid instance member signature:", pretty sig]
    where
      isTyped (Typed _ _) = True
      isTyped _ = False
      ts = map (\ (Typed _ t) -> t) args

typeOfTcFunDef :: TcFunDef -> Ty
typeOfTcFunDef (FunDef sig _) = typeOfTcSignature sig

pprRes :: Resolution -> Doc
-- type Resolution = (Ty, FunDef Id)
pprRes(ty, fd) = ppr ty <+> text ":" <+> text(shortName fd)

instance Pretty (Ty, FunDef Id) where
  ppr = pprRes

specmgu :: Ty -> Ty -> Either String TVSubst
specmgu (TyCon n ts) (TyCon n' ts')
  | n == n' && length ts == length ts' =
      specsolve (zip ts ts') mempty
specmgu (TyVar v) t = varBind v t
specmgu t (TyVar v) = varBind v t
specmgu t1 t2 = typesDoNotUnify t1 t2

varBind :: (MonadError String m) => Tyvar -> Ty -> m TVSubst
varBind v t
  | t == TyVar v = return mempty
  | v `elem` freetv t = infiniteTyErr v t
  | otherwise = do
      return (v |-> t)
  where
    infiniteTyErr v t = throwError $
      unwords
        [ "Cannot construct the infinite type:"
        , pretty v
        , "~"
        , pretty t
        ]

specsolve :: [(Ty, Ty)] -> TVSubst -> Either String TVSubst
specsolve [] s = pure s
specsolve ((t1, t2) : ts) s =
  do
    s1 <- specmgu (applytv s t1) (applytv s t2)
    s2 <- specsolve ts s1
    pure (s2 <> s1)

newtype TVSubst
  = TVSubst { unTVSubst :: [(Tyvar, Ty)] } deriving (Eq, Show)

restrict :: TVSubst -> [Tyvar] -> TVSubst
restrict (TVSubst s) vs
  = TVSubst [(v,t) | (v,t) <- s, v `notElem` vs]

emptyTVSubst :: TVSubst
emptyTVSubst = TVSubst []

-- composition operators

instance Semigroup TVSubst where
  s1 <> s2 = TVSubst (outer ++ inner)
    where
      outer = [(u, applytv s1 t) | (u, t) <- unTVSubst s2]
      inner = [(v,t) | (v,t) <- unTVSubst s1, v `notElem` dom2]
      dom2 = map fst (unTVSubst s2)

instance Monoid TVSubst where
  mempty = emptyTVSubst

(|->) :: Tyvar -> Ty -> TVSubst
u |-> t = TVSubst [(u, t)]

instance Pretty TVSubst where
  ppr = braces . commaSep . map go . unTVSubst
    where
      go (v,t) = ppr v <+> text "|->" <+> ppr t

class Data a => HasTV a where
  applytv :: TVSubst -> a -> a
  applytv s = everywhere (mkT (applytv @Ty s))

  freetv  :: a -> [Tyvar]    -- free variables
  freetv = everything (<>) (mkQ mempty (freetv @Ty))

  renametv :: a -> SM (a, TVSubst)
  renametv a = pure (a, mempty)

instance HasTV Ty where
  applytv (TVSubst s) t@(TyVar v)
    = maybe t id (lookup v s)
  applytv s (TyCon n ts)
    = TyCon n (applytv s ts)
  applytv _ t = t

  freetv (TyVar v@(TVar _)) = [v]
  freetv (TyCon _ ts) = freetv ts
  freetv _ = []

instance HasTV a => HasTV [a] where
    applytv s = map (applytv s)
    freetv = foldr (union . freetv) mempty

instance HasTV a => HasTV (Maybe a) where
  applytv s = fmap (applytv s)
  freetv = maybe [] freetv

instance (HasTV a, HasTV b) => HasTV (a,b) where  -- defaults

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

instance HasTV a => HasTV (Param a) where -- defaults
instance HasTV a => HasTV (Exp a) where  -- defaults
instance HasTV a => HasTV (Stmt a) where  -- defaults

instance HasTV (Pat Id) where


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
      pure(FunDef sig' body', subst)

addRenaming :: TVSubst -> Tyvar -> SM TVSubst
addRenaming b a = do
           fresh <- spNewName
           pure ( (a |-> TyVar (TVar fresh)) <> b )

-- TODO: refactor - make renametv return TVRenaming; turn rename* into class methods

newtype TVRenaming
  = TVR { unTVR :: [(Tyvar, Tyvar)] } deriving (Eq, Show)

instance Pretty TVRenaming where
  ppr = braces . commaSep . map go . unTVR
    where
      go (v,t) = ppr v <+> text "|->" <+> ppr t

toTVS :: TVRenaming -> TVSubst
toTVS = TVSubst . map (fmap TyVar) . unTVR

fromTVS :: TVSubst -> TVRenaming
fromTVS = TVR . map (fmap unTyVar) . unTVSubst where
    unTyVar (TyVar x) = x
    unTyVar t = error("fromTVS: " ++ pretty t ++ "is not a type variable")

renameTV :: TVRenaming -> Tyvar -> Tyvar
renameTV (TVR r) v = fromMaybe v (lookup v r)

renameTy :: TVRenaming -> Ty -> Ty
renameTy r = everywhere  (mkT (renameTV r))

renameSubst :: TVRenaming -> TVSubst -> TVSubst
renameSubst r = TVSubst . map rename . unTVSubst where
    rename (v, t) = (renameTV r v, renameTy r t)
