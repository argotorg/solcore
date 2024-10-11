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
generateDecls ((FunDef sig bd), sch) 
  = do
      dt <- lookupUniqueType (sigName sig)
      createInstance dt sig sch 
      pure ()

lookupUniqueType :: Name -> TcM (Maybe (TopDecl Id))
lookupUniqueType n = (fmap TDataDef . Map.lookup n) <$> gets uniqueTypes 

createInstance :: Maybe (TopDecl Id) -> Signature Id -> Scheme -> TcM () 
createInstance Nothing  _ _ = pure ()
createInstance (Just (TDataDef dt@(DataTy n vs _))) sig sch 
  = do
      argTys <- mapM tyParam (sigParams sig)
      let mainTy = TyCon n (TyVar <$> vs) 
          retTy = fromJust $ (sigReturn sig)
          ctx = sigContext sig
          ni = Name "invokable"
      bd <- createInvokeDef dt sig
      let instd = Instance ctx ni (mkArgTypes argTys retTy) mainTy [ bd ]
      addInstance (Name "invokable") (mkInstPred instd)
      writeDecl (TInstDef instd)
      pure ()

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
      argTys <- mapM tyParam (sigParams sig)
      (args, mp) <- mkParamForSig argTys (TyCon n (TyVar <$> vs))
      let ctx = sigContext sig 
          ni = QualName (Name "invokable") "invoke"
          vs = fv argTys `union` maybe [] fv (sigReturn sig)
      pure (Signature vs ctx ni args (sigReturn sig), mp) 

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
        ret = if null patTys then Return $ Call Nothing cid [Var pid] 
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


