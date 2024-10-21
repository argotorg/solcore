module Solcore.Frontend.TypeInference.TcInvokeGen where 

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.State

import Data.Generics hiding (Constr)
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives

-- generate invoke instances for functions

generateDecls :: (FunDef Name, Scheme) -> TcM () 
generateDecls (fd@(FunDef sig bd), sch) 
  = do
      createTypeFunDef fd 
      dt <- lookupUniqueType (sigName sig)
      createInstance dt sig sch 
      pure ()

lookupUniqueType :: Name -> TcM (Maybe (TopDecl Name))
lookupUniqueType n = (fmap TDataDef . Map.lookup n) <$> gets uniqueTypes 

createInstance :: Maybe (TopDecl Name) -> Signature Name -> Scheme -> TcM () 
createInstance Nothing  _ _ = pure ()
createInstance (Just (TDataDef dt@(DataTy n vs _))) sig sch 
  = do
      argTys' <- createArgs sig 
      let mainTy = TyCon n ([argTys', retTy])
          retTy = fromJust $ (sigReturn sig)
          ni = Name "invokable"
      bd <- createInvokeDef dt sig
      let instd = Instance [] ni ([argTys', retTy]) mainTy [ bd ]
      addInstance (Name "invokable") (mkInstPred instd)
      writeInstance instd
      pure ()

notUniqueTyArg :: Ty -> TcM Bool 
notUniqueTyArg (TyVar _) = pure True 
notUniqueTyArg (TyCon n _)
  = do 
      uniques <- Map.elems <$> gets uniqueTypes
      pure (all (\ d -> dataName d /= n) uniques)

mkInstPred :: Instance Name -> Inst
mkInstPred (Instance ctx n ts t _) 
  = anfInstance $ ctx :=> InCls n t ts

anfInstance :: Inst -> Inst
anfInstance inst@(q :=> p@(InCls c t [])) = inst
anfInstance inst@(q :=> p@(InCls c t as)) = q ++ q' :=> InCls c t bs 
  where
    q' = zipWith (:~:) bs as
    bs = map TyVar $ take (length as) freshNames
    tvs = fv inst
    freshNames = filter (not . flip elem tvs) (flip TVar False <$> namePool)

createInvokeDef :: DataTy -> Signature Name -> TcM (FunDef Name)
createInvokeDef dt sig 
  = do 
      (sig', mi) <- createInvokeSig dt sig 
      bd <- createInvokeBody dt sig mi  
      pure (FunDef sig' bd)

createInvokeSig :: DataTy -> Signature Name -> TcM (Signature Name, Maybe Id)
createInvokeSig (DataTy n vs cons) sig 
  = do
      argTys' <- createArgs sig
      let retTy = fromJust $ sigReturn sig
      (args, mp) <- mkParamForSig argTys' (TyCon n ([argTys', retTy]))
      let  
          ni = Name "invoke"
          vs = fv argTys' `union` maybe [] fv (sigReturn sig)
      pure (Signature vs [] ni args (sigReturn sig), mp) 

createArgs :: Signature Name -> TcM Ty 
createArgs sig 
  = do 
      argTys <- mapM tyParam (sigParams sig)
      argTys' <- filterM notUniqueTyArg argTys
      pure $ tupleTyFromList argTys'

tupleTyFromList :: [Ty] -> Ty
tupleTyFromList [] = unit 
tupleTyFromList [t] = t 
tupleTyFromList [t1,t2] = pair t1 t2 
tupleTyFromList (t1 : ts) = pair t1 (tupleTyFromList ts)

mkParamForSig :: Ty -> Ty -> TcM ([Param Name], Maybe Id)
mkParamForSig argTy selfTy 
  = do 
      let selfArg = Typed (Name "self") selfTy 
      paramName <- freshName 
      pure ([selfArg, Typed paramName argTy], Just (Id paramName argTy))


createInvokeBody :: DataTy -> Signature Name -> Maybe Id -> TcM (Body Name)
createInvokeBody dt sig Nothing
  = do
      cn <- gets contract  
      let 
          cid = if isNothing cn then sigName sig
                else QualName (fromJust cn) 
                              (pretty $ sigName sig)
          cexp = Call Nothing cid []
      pure [Return cexp]
createInvokeBody dt sig (Just pid)
  = createInvokeMatch dt sig pid 

createInvokeMatch :: DataTy -> Signature Name -> Id -> TcM (Body Name)
createInvokeMatch (DataTy _ _ [Constr _ targs]) sig (Id pid ty)
  = do
      cn <- gets contract 
      argTys <- mapM tyParam (sigParams sig)
      let patTys = tyConArgs ty 
          retTy = fromJust $ sigReturn sig 
          cname = if isNothing cn || isQual (sigName sig) then sigName sig
                  else QualName (fromJust cn) 
                                (pretty $ sigName sig)
      (pats, ns) <- unzip <$> mapM (const mkPat) patTys
      let 
        ret = if null argTys then Return $ Call Nothing cname [] 
              else if null patTys then 
                if null targs then Return $ Call Nothing cname [Var pid] 
                else Return $ Call Nothing cname [Var (Name "self"), Var pid]
              else 
                if null targs then Return $ Call Nothing cname (Var <$> ns)
                else Return $ Call Nothing cname (Var <$> ((Name "self") : ns))
        pat = if null patTys then PVar pid else foldr1 ppair pats 
        stmt = if null patTys then [ret] else [Match [Var pid] [([pat], [ret])]]
      pure stmt 

isQual :: Name -> Bool
isQual (QualName _ _) = True 
isQual _ = False

tyConArgs :: Ty -> [Ty]
tyConArgs (TyVar _) = []
tyConArgs (TyCon _ tys) = tys

ppair :: Pat Name -> Pat Name -> Pat Name  
ppair p1 p2 
  = PCon pairCon [p1, p2] 
    where 
      pairCon = Name "pair" 
      

mkPat :: TcM (Pat Name, Name)
mkPat = do 
  n <- freshName 
  pure (PVar n, n)


mkArgTypes :: [Ty] -> Ty -> [Ty] 
mkArgTypes args ret 
  = case mkArgTy args of 
      Just arg' -> [arg', ret] 
      Nothing -> [ret] 

mkArgTy :: [Ty] -> Maybe Ty 
mkArgTy [] = Nothing 
mkArgTy [arg] = Just arg 
mkArgTy (arg : args) 
  = case mkArgTy args of 
      Just arg' -> Just (pair arg arg')
      Nothing -> Just arg 

tyParam :: Param a -> TcM Ty 
tyParam (Typed _ t) = pure t 
tyParam (Untyped _) = freshTyVar

-- creating unique types 

createTypeFunDef :: FunDef Name -> TcM (FunDef Name)
createTypeFunDef (FunDef sig bdy)
  = do 
      dt <- lookupUniqueType (sigName sig)
      case dt of 
        Nothing -> do 
          sig' <- freshSignature sig  
          createUniqueType' (sigName sig) (schemeFromSig sig')
          pure (FunDef sig bdy)
        Just _ -> pure (FunDef sig bdy)

freshSignature :: Signature Name -> TcM (Signature Name)
freshSignature sig 
  = do 
      let ctx = sigContext sig 
          args = sigParams sig 
          ret = sigReturn sig
          fvArg (Typed _ ty) = fv ty 
          fvArg _ = []
          fv' = foldr (union . fvArg) []
          vs = vars $ fv ctx `union` fv' args `union` fv ret
      args' <- mapM freshParam args 
      ret' <- freshReturn ret 
      pure sig {
             sigParams = args' 
           , sigReturn = ret'
           } 

freshParam :: Param Name -> TcM (Param Name)
freshParam p@(Typed _ _) = pure p 
freshParam (Untyped n) 
  = do 
      v <- freshTyVar
      pure (Typed n v)

freshReturn :: Maybe Ty -> TcM (Maybe Ty)
freshReturn Nothing 
  = do 
      v <- freshTyVar
      pure (Just v)
freshReturn p = pure p 

vars :: [Tyvar] -> [Name]
vars = map (\ (TVar n _) -> n) 

fv' :: [Param Id] -> [Tyvar]
fv' = foldr step []
  where 
    step (Typed _ ty) ac = fv ty `union` ac 
    step (Untyped _) ac = ac 

schemeFromSig :: Signature Name -> Scheme 
schemeFromSig sig 
  = let 
      ctx = sigContext sig 
      argTys = map tyFromParam (sigParams sig)
      retTy = fromJust $ sigReturn sig 
      ty = funtype argTys retTy 
      vs = fv (ctx :=> ty)
    in Forall vs (ctx :=> ty)

tyFromParam :: Param Name -> Ty 
tyFromParam (Typed _ ty) = ty 

createUniqueType' :: Name -> Scheme -> TcM ()
createUniqueType' n (Forall vs _)
     = do
        dt <- uniqueType n vs 
        writeDataTy dt
        checkDataType dt 

uniqueType :: Name -> [Tyvar] -> TcM DataTy 
uniqueType n vs
  = do 
      m <- incCounter
      let 
          argVar = TVar (Name "args") False 
          retVar = TVar (Name "ret") False 
          nt = Name $ "t_" ++ pretty n ++ show m
          dc = Constr nt []
          dt = DataTy nt [argVar, retVar] [dc]
      modify (\ env -> env {uniqueTypes = Map.insert n dt (uniqueTypes env)})
      pure dt


