-- {-# LANGUAGE DefaultSignatures #-}
module Solcore.Backend.Specialise(specialiseCompUnit) where
{- * Specialisation
Create specialised versions of polymorphic and overloaded functions.
This is meant to be run on typed and defunctionalised code, so no higher-order functions.
-}

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.List(intercalate)
import qualified Data.Map as Map

import Common.Monad
import Solcore.Backend.Retype
import Solcore.Backend.SpecMonad
import Solcore.Desugarer.IfDesugarer(desugaredBoolTy)
import Solcore.Frontend.Pretty.ShortName
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax hiding(name, decls)
import Solcore.Frontend.TypeInference.Id ( Id(..) )
import Solcore.Frontend.TypeInference.TcEnv(TcEnv)
import Solcore.Primitives.Primitives

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
    deployDecls <- case findConstructor decls of
      Just c -> withLocalState do
        _cname' <- specConstructor c
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
specialiseTopDecl _ = pure []

findConstructor :: [ContractDecl Id] -> Maybe (Constructor Id)
findConstructor = foldr (\d -> (getConstructor d <|>)) Nothing

getConstructor :: ContractDecl Id -> Maybe (Constructor Id)
getConstructor (CConstrDecl c) = Just c
getConstructor _ = Nothing


specEntry :: Name -> SM ()
specEntry name = withLocalState do
    let anytype = TyVar (TVar (Name "any"))
    mres <- lookupResolution name anytype
    case mres of
      Just (fd, ty, subst) -> do
        debug ["< resolution: ", show name, " : ", pretty ty, "@", pretty subst]
        void(specFunDef fd)
      Nothing -> do
        warns ["!! Warning: no resolution found for ", show name]

specConstructor :: Constructor Id -> SM Name
specConstructor (Constructor [] body) = do
  let sig = Signature [] [] (Name "constructor") [] (Just unit)
  let fd = FunDef sig body
  specFunDef fd
specConstructor _ = error "Unsupported constructor"

addContractResolutions :: Contract Id -> SM ()
addContractResolutions (Contract _name _args cdecls) =
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
specExp (Call Nothing i args) ty = do
  -- debug ["> specExp (Call): ", pretty e, " : ", pretty (idType i), " ~> ", pretty ty]
  (i', args') <- specCall i args ty
  let e' = Call Nothing i' args'
  -- debug ["< specExp (Call): ", pretty e']
  return e'
specExp (Con i es) ty = do
  -- let t = typeOfTcExp e
  -- debug ["> specConApp: ", pretty e, " : ", pretty t, " ~> ", pretty ty]
  (i' , es') <- specConApp i es ty
  let e' = Con i' es'
  return e'
specExp (Cond e1 e2 e3) ty = do
  e1' <- specExp e1 desugaredBoolTy
  e2' <- specExp e2 ty
  e3' <- specExp e3 ty
  pure (Cond e1' e2' e3')
specExp (Var (Id n _t)) ty = pure (Var (Id n ty))
specExp e@FieldAccess{} _ty = error("Specialise: FieldAccess not implemented for" ++ pretty e)
specExp (TyExp e1 _) ty = specExp e1 ty
specExp e _ty = atCurrentSubst e -- FIXME

specConApp :: Id -> [TcExp] -> Ty -> SM (Id, [TcExp])
-- specConApp i@(Id n conTy) [] ty = pure (i, [])
specConApp i@(Id _n conTy) args ty = do
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
specCall i@(Id (Name "revert") _e) args _ty = pure (i, args)  -- FIXME
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
      debug ["< resolution: ", show name, "~>", shortName fd, " : ", pretty ty, "@", pretty phi]
      extSpSubst phi
      subst <- getSpSubst
      let ty'' = applytv subst fty
      ensureClosed ty'' (Call Nothing i args) subst
      name' <- specFunDef fd
      debug ["< specCall: ", pretty name']
      args'' <- atCurrentSubst args'
      return (Id name' ty'', args'')
    Nothing -> do
      void(panics ["! specCall: no resolution found for ", show name, " : ", pretty funType])
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
  (fd, renaming) <- renametv fd0
  let sig0 = funSignature fd0
  let sig1 = funSignature fd
  subst <- renameSubst renaming <$> getSpSubst
  putSpSubst subst
  debug ["> specFunDef raw input: ", pretty sig0, " renaming", pretty renaming, " subst=", pretty subst]
  let sig' = applytv subst sig1 { sigVars = [], sigContext = [] }
  let name = sigName sig1
  let funType = typeOfTcFunDef fd
  let tvs = freetv funType
  let tvs' = applytv subst (map TyVar tvs)
  debug ["> specFunDef ", pretty name, " : ", pretty sig1,  " tvs'=", prettys tvs', " subst=", pretty subst]
  let name' = specName name tvs'
  let ty' = applytv subst funType
  mspec <- lookupSpecialisation name'
  case mspec of
    Just _ -> return name'
    Nothing -> do
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
  exps' <- specScruts exps
  alts' <- forM alts specAlt
  return $ Match exps' alts'
  where
    specAlt (pat, body) = do
      body' <- specBody body
      pat' <- atCurrentSubst pat
      return (pat', body')
    specScruts = mapM specScrut
    specScrut e = do
      ty <- atCurrentSubst (typeOfTcExp e)
      e' <- specExp e ty
      return e'


specName :: Name -> [Ty] -> Name
specName n [] = Name $ flattenQual n
specName n ts = Name $ flattenQual n ++ "$" ++ intercalate "_" (map mangleTy ts)

flattenQual :: Name -> String
flattenQual (Name n) = n
flattenQual (QualName n s) = flattenQual n ++ "_" ++ s

mangleTy :: Ty -> String
mangleTy (TyVar _) = ""
mangleTy (TyCon (Name "()") []) = "unit"
mangleTy (TyCon (Name n) []) = n
mangleTy (TyCon n ts) = flattenQual n ++ embrace mantys where
    mantys = filter (not . null) (map mangleTy ts)
    embrace [] = ""
    embrace xs = "L" ++ intercalate "_" xs ++ "J"
mangleTy t = error "Specialise: mangleTy not implemented for " ++ show t

prettyId :: Id -> String
prettyId = render . pprId

pprId :: Id -> Doc
pprId (Id n t@TyVar{}) = ppr n <> text "@" <> ppr t
pprId (Id n t@TyCon{}) = ppr n <> "@" <> ppr t
pprId (Id n t) = ppr n <> text "@" <> parens(ppr t)

pprConApp :: Id -> [TcExp] -> Doc
pprConApp i args = pprId i <> brackets (commaSepList args)

prettyConApp :: Id -> [TcExp] -> String
prettyConApp i args = render (pprConApp i args)
