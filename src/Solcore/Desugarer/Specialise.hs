module Solcore.Desugarer.Specialise where  --(specialiseCompUnit, typeOfTcExp) where
{- * Specialisation
Create specialised versions of polymorphic and overloaded (TODO) functions.
This is meant to be run on typed and defunctionalised code, so no higher-order functions.
-}

import Common.Monad
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics
import Data.List(intercalate)
import qualified Data.Map as Map
import GHC.Stack
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id ( Id(..) )
import Solcore.Frontend.TypeInference.TcEnv(TcEnv(..),TypeInfo(..))
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
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
  , spSubst :: Subst
  , spDebug :: Bool
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
-- local changes are discarded, with the exception of the `specTable`
withLocalState :: SM a -> SM a
withLocalState m = do
    s <- get
    a <- m
    spTable <- gets specTable
    put s
    modify $ \s -> s { specTable = spTable }
    return a

initSpecState :: Bool ->TcEnv -> SpecState
initSpecState debugp env = SpecState
    { spResTable = emptyTable
    , specTable = emptyTable
    , spTypeTable = typeTable env
    , spDataTable = Map.empty
    , spGlobalEnv = env
    , splocalEnv = emptyTable
    , spSubst = emptySubst
    , spDebug = debugp
    }

-- make type variables flexible by replacing them with metas
flex :: Ty -> Ty
flex (TyVar (TVar n)) = Meta (MetaTv n)
flex (TyCon cn tys) = TyCon cn (map flex tys)
flex t = t

-- make all type variables flexible in a syntactic construct
flexAll :: Data a => a -> a
flexAll = everywhere (mkT flex)

addSpecialisation :: Name -> TcFunDef -> SM ()
addSpecialisation name fd = modify $ \s -> s { specTable = Map.insert name fd (specTable s) }

lookupSpecialisation :: Name -> SM (Maybe TcFunDef)
lookupSpecialisation name = gets (Map.lookup name . specTable)

addResolution :: Name -> Ty -> TcFunDef -> SM ()
addResolution name ty fun = do
    modify $ \s -> s { spResTable = Map.insertWith (++) name [(flex ty, fun)] (spResTable s) }

lookupResolution :: Name -> Ty ->  SM (Maybe (TcFunDef, Ty, Subst))
lookupResolution name ty = gets (Map.lookup name . spResTable) >>= findMatch ty where
  str :: Pretty a => a -> String
  str = pretty
  findMatch :: Ty -> Maybe [Resolution] -> SM (Maybe (TcFunDef, Ty, Subst))
  findMatch etyp (Just res) = do
    debug ["|> findMatch ", pretty etyp, " in ", prettys res]
    firstMatch etyp res
  findMatch _ Nothing = return Nothing
  firstMatch :: Ty -> [Resolution] -> SM (Maybe (TcFunDef, Ty, Subst))
  firstMatch etyp [] = return Nothing
  firstMatch etyp ((t,e):rest)
    | Right subst <- mgu t etyp = do  -- TESTME: match is to weak for MPTC, but isn't mgu too strong?
        debug ["< lookupRes - match found for ", str name, ": ", str t, " ~ ", str etyp, " => ", str subst]
        return (Just (e, t, subst))
    | otherwise = firstMatch etyp rest

getSpSubst :: SM Subst
getSpSubst = gets spSubst

extSpSubst :: Subst -> SM ()
extSpSubst subst = modify $ \s -> s { spSubst =  spSubst s <> subst }

atCurrentSubst :: HasType a => a -> SM a
atCurrentSubst a = flip apply a <$> getSpSubst

addData :: DataTy -> SM ()
addData dt = modify (\s -> s { spDataTable = Map.insert (dataName dt) dt (spDataTable s) })

-------------------------------------------------------------------------------

specialiseCompUnit :: CompUnit Id -> Bool -> TcEnv -> IO (CompUnit Id)
specialiseCompUnit compUnit debugp env = runSM debugp env do
    addGlobalResolutions compUnit
    contracts' <- forM (contracts compUnit) specialiseContract
    return $ compUnit { contracts = contracts' }

addGlobalResolutions :: CompUnit Id -> SM ()
addGlobalResolutions compUnit = forM_ (contracts compUnit) addDeclResolutions

addDeclResolutions :: TopDecl Id -> SM ()
addDeclResolutions (TInstDef inst) = addInstResolutions (flexAll inst)
addDeclResolutions (TFunDef fd) = addFunDefResolution (flexAll fd)
addDeclResolutions (TDataDef dt) = addData dt
addDeclResolutions _ = return ()


addInstResolutions :: Instance Id -> SM ()
addInstResolutions inst = forM_ (instFunctions inst) (addMethodResolution (mainTy inst))

specialiseContract :: TopDecl Id -> SM (TopDecl Id)
specialiseContract (TContr (Contract name args decls)) = withLocalState do
    addContractResolutions (Contract name args decls)
    forM_ entries (specEntry . flexAll)
    st <- gets specTable
    dt <- gets spDataTable
    let dataDecls = map (CDataDecl . snd) (Map.toList dt)
    let funDecls = map (CFunDecl . snd) (Map.toList st)
    let decls' = dataDecls ++ funDecls
    return (TContr (Contract name args decls'))
    where
      entries = ["main"]    -- Eventually all public methods
      -- flexDecls = flexAll decls

specialiseContract decl = pure decl

specEntry :: Name -> SM ()
specEntry name = withLocalState do
    let any = MetaTv (Name "any")
    let anytype = Meta any
    mres <- lookupResolution name anytype
    case mres of
      Just (fd, ty, subst) -> do
        debug ["< resolution: ", show name, " : ", pretty ty, "@", pretty subst]
        void(specFunDef fd)
      Nothing -> do
        warns ["!! Warning: no resolution found for ", show name]

addContractResolutions :: Contract Id -> SM ()
addContractResolutions (Contract name args decls) = do
  forM_ decls addCDeclResolution

addCDeclResolution :: ContractDecl Id -> SM ()
addCDeclResolution (CFunDecl fd) = addFunDefResolution (flexAll fd)
addCDeclResolution (CDataDecl dt) = addData dt
addCDeclResolution _ = return ()

addFunDefResolution fd = do
  let sig = funSignature fd
  let name = sigName sig
  let funType = typeOfTcFunDef fd
  addResolution name funType fd
  debug ["+ addDeclResolution: ", show name, " : ", pretty funType]

addMethodResolution :: Ty -> TcFunDef -> SM ()
addMethodResolution ty fd = do
  let sig = funSignature fd
  let name = sigName sig
  let name' = specName name [ty]
  let funType = typeOfTcFunDef fd
  let fd' = FunDef sig{sigName = name'} (funDefBody fd)
  addResolution name funType fd'
  debug ["+ addMethodResolution: ", show name', " : ", pretty funType]

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

specExp e@(Var (Id n t)) ty = pure (Var (Id n ty))
specExp e@(FieldAccess me fld) ty = error("Specialise: FieldAccess not implemented for" ++ pretty e)
specExp e@(TyExp e1 _) ty = specExp e1 ty
specExp e ty = atCurrentSubst e -- FIXME

specConApp :: Id -> [TcExp] -> Ty -> SM (Id, [TcExp])
-- specConApp i@(Id n conTy) [] ty = pure (i, [])
specConApp i@(Id n conTy) args ty = do
  subst <- getSpSubst
  let argTypes = map typeOfTcExp args
  let argTypes' = apply subst argTypes
  let i' = apply subst i
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
      let ty' = apply subst ty
      ensureClosed ty' (Call Nothing i args) subst
      name' <- specFunDef fd
      debug ["< specCall: ", pretty name']
      args'' <- atCurrentSubst args'
      return (Id name' ty', args'')
    Nothing -> do
      debug ["! specCall: no resolution found for ", show name, " : ", pretty funType]
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
specFunDef fd = withLocalState do
  subst <- getSpSubst
  let sig = funSignature fd
  let name = sigName sig
  let funType = typeOfTcFunDef fd
  let tvs = fv funType
  let mvs = mv funType
  let tvs' = apply subst (map Meta mvs)
  debug ["> specFunDef ", pretty name, " : ", pretty funType, " mvs=", prettys mvs, " tvs'=", prettys tvs']
  let name' = specName name tvs'
  let ty' = apply subst funType
  mspec <- lookupSpecialisation name'
  case mspec of
    Just fd' -> return name'
    Nothing -> do
      let sig' = apply subst (funSignature fd)
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
ensureClosed :: Pretty a => Ty -> a -> Subst ->  SM ()
ensureClosed ty ctxt subst = do
  let tvs = fv ty
  unless (null tvs) $ panics ["spec(", pretty ctxt,"): free type vars in ", pretty ty, ": ", show tvs
                             , " @ subst=", pretty subst]
  let mvs = mv ty
  unless (null tvs) $ panics ["spec(", pretty ctxt,"): free meta vars in ", pretty ty, ": ", show mvs
                             , " @ subst=", pretty subst]

specStmt :: Stmt Id -> SM(Stmt Id)
specStmt stmt@(Return e) = do
  subst <- getSpSubst
  let ty = typeOfTcExp e
  let ty' = apply subst ty
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

typeOfTcFunDef :: TcFunDef -> Ty
typeOfTcFunDef (FunDef sig _) = flexAll $ typeOfTcSignature sig

pprRes :: Resolution -> Doc
-- type Resolution = (Ty, FunDef Id)
pprRes(ty, fd) = ppr ty <+> text ":" <+> text(shortName fd)

instance Pretty (Ty, FunDef Id) where
  ppr = pprRes
