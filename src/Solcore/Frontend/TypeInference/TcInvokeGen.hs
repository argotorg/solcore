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

generateDecls :: (FunDef Id, Scheme) -> TcM () 
generateDecls (fd@(FunDef sig bd), sch) 
  = do
      createTypeFunDef fd 
      dt <- lookupUniqueType (sigName sig)
      createInstance dt sig sch 
      pure ()

lookupUniqueType :: Name -> TcM (Maybe (TopDecl Id))
lookupUniqueType n = (fmap TDataDef . Map.lookup n) <$> gets uniqueTypes 

createInstance :: Maybe (TopDecl Id) -> Signature Id -> Scheme -> TcM () 
createInstance Nothing  _ _ = pure ()
createInstance (Just (TDataDef dt@(DataTy n vs _))) sig sch 
  = do
      argTys' <- createArgs sig 
      let mainTy = TyCon n (argTys' ++ [retTy])
          retTy = fromJust $ (sigReturn sig)
          ctx = sigContext sig
          ni = Name "invokable"
      bd <- createInvokeDef dt sig
      let instd = Instance ctx ni (argTys'  ++ [retTy]) mainTy [ bd ]
      addInstance (Name "invokable") (mkInstPred instd)
      writeDecl (TInstDef instd)
      pure ()

notUniqueTyArg :: Ty -> TcM Bool 
notUniqueTyArg (TyVar _) = pure True 
notUniqueTyArg (TyCon n _)
  = do 
      uniques <- Map.elems <$> gets uniqueTypes
      pure (all (\ d -> dataName d /= n) uniques)

mkInstPred :: Instance Id -> Inst
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

createInvokeDef :: DataTy -> Signature Id -> TcM (FunDef Id)
createInvokeDef dt sig 
  = do 
      (sig', mi) <- createInvokeSig dt sig 
      bd <- createInvokeBody dt sig mi  
      pure (FunDef sig' bd)


createInvokeSig :: DataTy -> Signature Id -> TcM (Signature Id, Maybe Id)
createInvokeSig (DataTy n vs cons) sig 
  = do
      argTys' <- createArgs sig
      let retTy = fromJust $ sigReturn sig
      (args, mp) <- mkParamForSig argTys' (TyCon n (argTys' ++ [retTy]))
      let ctx = sigContext sig 
          ni = QualName (Name "invokable") "invoke"
          vs = fv argTys' `union` maybe [] fv (sigReturn sig)
      pure (Signature vs ctx ni args (sigReturn sig), mp) 

createArgs :: Signature Id -> TcM [Ty] 
createArgs sig 
  = do 
      argTys <- mapM tyParam (sigParams sig)
      argTys' <- filterM notUniqueTyArg argTys
      if null argTys' then pure [unit] else pure argTys'

mkParamForSig :: [Ty] -> Ty -> TcM ([Param Id], Maybe Id)
mkParamForSig argTys selfTy 
  = do 
      let selfArg = Typed (Id (Name "self") selfTy) selfTy 
      case mkArgTy argTys of 
        Just arg -> do 
          paramName <- freshName 
          let pid = Id paramName arg  
          pure ([selfArg, Typed pid arg], Just pid)
        Nothing -> pure ([selfArg], Nothing)


createInvokeBody :: DataTy -> Signature Id -> Maybe Id -> TcM (Body Id)
createInvokeBody _ sig Nothing
  = do
      let 
          retTy = fromJust $ sigReturn sig 
          cid = Id (sigName sig) retTy
          cexp = Call Nothing cid []
      pure [Return cexp]
createInvokeBody dt sig (Just pid)
  = createInvokeMatch sig pid 

createInvokeMatch :: Signature Id -> Id -> TcM (Body Id)
createInvokeMatch sig pid@(Id _ ty)
  = do
      argTys <- mapM tyParam (sigParams sig)
      let patTys = tyConArgs ty 
          retTy = fromJust $ sigReturn sig 
          cid = Id (sigName sig) (foldr (:->) retTy argTys)
      (pats, ns) <- unzip <$> mapM mkPat patTys
      let 
        ret = if null argTys then Return $ Call Nothing cid [] 
              else if null patTys then Return $ Call Nothing cid [Var pid] 
                                  else Return $ Call Nothing cid (Var <$> ns)
        pat = if null patTys then PVar pid else foldr1 ppair pats 
        stmt = if null patTys then [ret] else [Match [Var pid] [([pat], [ret])]]
      pure stmt 

tyConArgs :: Ty -> [Ty]
tyConArgs (TyVar _) = []
tyConArgs (TyCon _ tys) = tys

ppair :: Pat Id -> Pat Id -> Pat Id 
ppair p1 p2 
  = PCon pairCon [p1, p2] 
    where 
      pairCon = Id (Name "pair") pairTy 
      pairTy = pair (tyFrom p1) (tyFrom p2)
      tyFrom (PVar (Id _ t)) = t
      tyFrom (PCon (Id _ t) _) = t 
      

mkPat :: Ty -> TcM (Pat Id, Id)
mkPat ty = do 
  n <- freshName 
  let pid = Id n ty 
  pure (PVar pid, pid)


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

createTypeFunDef :: FunDef Id -> TcM (FunDef Id)
createTypeFunDef (FunDef sig bdy)
  = do 
      sig' <- freshSignature sig  
      createUniqueType' (sigName sig) (schemeFromSig sig')
      pure (FunDef sig bdy)

freshSignature :: Signature Id -> TcM (Signature Id)
freshSignature sig 
  = do 
      let ctx = sigContext sig 
          args = sigParams sig 
          ret = sigReturn sig 
          vs = vars $ fv ctx `union` fv' args `union` fv ret
      args' <- mapM freshParam args 
      ret' <- freshReturn ret 
      pure sig {
             sigParams = args' 
           , sigReturn = ret'
           } 

freshParam :: Param Id -> TcM (Param Id)
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

schemeFromSig :: Signature Id -> Scheme 
schemeFromSig sig 
  = let 
      ctx = sigContext sig 
      argTys = map tyFromParam (sigParams sig)
      retTy = fromJust $ sigReturn sig 
      ty = funtype argTys retTy 
      vs = fv (ctx :=> ty)
    in Forall vs (ctx :=> ty)

tyFromParam :: Param Id -> Ty 
tyFromParam (Typed _ ty) = ty 

createUniqueType' :: Name -> Scheme -> TcM ()
createUniqueType' n (Forall vs _)
     = do
        dt <- uniqueType n vs 
        writeDecl (TDataDef dt)
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


