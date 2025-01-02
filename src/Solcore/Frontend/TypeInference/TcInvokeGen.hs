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

generateDecls :: (FunDef Id, Scheme) -> TcM (DataTy, Instance Name) 
generateDecls (fd@(FunDef sig bd), sch) 
  = do
      liftIO $ putStrLn $ "Function type:" ++ pretty sch
      dt <- lookupUniqueType (sigName sig) sch
      liftIO $ putStrLn $ "Unique type:" ++ pretty dt
      instd <- createInstance dt sig sch 
      pure (dt, instd)

lookupUniqueType :: Name -> Scheme -> TcM DataTy
lookupUniqueType n sch 
  = do 
      dt <- (fromJust . Map.lookup n) <$> gets uniqueTypes
      if isClosureData dt then 
        genClosureData n dt sch 
      else pure dt 

genClosureData :: Name -> DataTy -> Scheme -> TcM DataTy
genClosureData nm dt@(DataTy dn vs (Constr n ((TyCon cn ts) : _) : _)) (Forall vs' _) 
  = do 
      zs' <- mapM (const freshName) vs'
      let 
          toTVar n = TVar n False
          toTyVar n = TyVar (toTVar n)
          ts2 = map toTyVar zs'
          s2 = Subst (zip vs' ts2)
          vs1 = map toTVar zs' 
          ts' = TyVar <$> take (length (fv ts)) vs1 
          sch = Forall vs1 ([] :=> funtype ts' (TyCon dn (TyVar <$> vs1)))
          dt1 = DataTy dn vs1 [Constr n [TyCon cn ts']]
      addUniqueType nm dt1
      pure dt1 

isClosureData :: DataTy -> Bool 
isClosureData (DataTy _ _ ((Constr _ ts) : _)) 
  = not $ null ts 
isClosureData _ = False

createInstance :: DataTy -> Signature Id -> Scheme -> TcM (Instance Name) 
createInstance dt@(DataTy n vs c) sig sch 
  = do
      (ps :=> t) <- freshInst sch
      (argTys', tys, retTy) <- createArgs t   
      let
        vs' = (fv (ps :=> t)) 
        args = if isClosureData dt then (TyVar <$> vs') 
               else [argTys', retTy]
      let mainTy = TyCon n args
      bd <- createInvokeDef dt sig ps t
      let instd = Instance ps invokableName [argTys', retTy] mainTy [ bd ]
      pure instd

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

createInvokeDef :: DataTy -> Signature Id -> [Pred] -> Ty -> TcM (FunDef Name)
createInvokeDef dt sig ps t 
  = do
      (sig', mi) <- createInvokeSig dt sig ps t 
      bd <- createInvokeBody dt sig mi t 
      pure (FunDef sig' bd)

createInvokeSig :: DataTy -> Signature Id -> [Pred] -> Ty -> TcM (Signature Name, Id)
createInvokeSig dt@(DataTy n vs cons) sig ps t 
  = do
      (argTys', tys, retTy) <- createArgs t
      let
        vs' = fv ps \\ fv t 
        args = if isClosureData dt then tys ++ (retTy : (TyVar <$> vs')) 
               else [argTys', retTy]
      (args', mp) <- mkParamForSig argTys' (TyCon n args)
      let  
          vs = fv [retTy, argTys']
          sig' = Signature vs ps invokeName args' (Just retTy)
      pure (sig', mp)

createArgs :: Ty -> TcM (Ty, [Ty], Ty)
createArgs t  
  = do
      let (ts',t') = splitTy t 
      pure $ (tupleTyFromList ts', ts', t')

tupleTyFromList :: [Ty] -> Ty
tupleTyFromList [] = unit 
tupleTyFromList [t] = t 
tupleTyFromList [t1,t2] = pair t1 t2 
tupleTyFromList (t1 : ts) = pair t1 (tupleTyFromList ts)

mkParamForSig :: Ty -> Ty -> TcM ([Param Name], Id)
mkParamForSig argTy selfTy 
  = do 
      let selfArg = Untyped selfName 
      paramName <- freshName 
      let pid = Id paramName argTy
      pure ([selfArg, Untyped paramName], pid)

-- creating invoke body 

-- no pattern matching needed: just make a call to the function 
-- pattern matching needed:
-- * No closure: function receives more than one argument, which are passed as a tuple 
-- * Closure: pattern match on closure to get the parameters and make the call with arguments.

createInvokeBody :: DataTy -> Signature Id -> Id -> Ty -> TcM (Body Name)
createInvokeBody (DataTy dt vs [Constr c1 targs]) sig x@(Id pid ty) t 
  = do
      let patTys = tyConArgs ty
          cname = sigName sig
          (argTys, retTy) = splitTy t 
      (pats, ns) <- unzip <$> mapM mkPat patTys
      if [ty] == argTys && null targs then 
        -- no need to match the closure type and arguments tuple 
        pure [Return $ Call Nothing cname [Var pid]]
      else if null targs then do
        -- no pattern matching on closure needed
        let args = if null argTys then [] else [Var pid]
        if null patTys then 
          -- no pattern matching on arguments tuple
          pure [Return $ Call Nothing cname args]
        else do 
          -- pattern matching on arguments tuple 
          let pats' = foldr1 ppair pats 
              ret = Return $ Call Nothing cname (concat ns)
          pure [Match [Var pid] [([pats'], [ret])]]
      else do 
        (cps, pvs) <- unzip <$> mapM (\ _ -> let 
                                  g n = (PVar n, Var n) in g <$> freshName) 
                                     targs 
        -- matching on closure needed
        let n1 = selfName
            cp = Typed n1 tc
            cpat = PCon c1 cps
            pats1 = if null pats then [PVar pid] else pats 
            ns1 = if null ns then [Var n1, Var pid] else concat ns 
            pats' = foldr1 ppair (cpat : pats1) 
            ret = Return $ Call Nothing cname (pvs ++ ns1)
            tc = TyCon dt (TyVar <$> vs)
        pure [Match [epair (Var n1) (Var pid)] [([pats'], [ret])]]


isQual :: Name -> Bool
isQual (QualName _ _) = True 
isQual _ = False

tyConArgs :: Ty -> [Ty]
tyConArgs (TyVar _) = []
tyConArgs (TyCon _ tys) = tys

ppair :: Pat Name -> Pat Name -> Pat Name  
ppair p1 p2 
  = PCon pairCon [p1, p2] 

-- invariant: We should remove wildcards before.

tyFromPat :: Pat Id -> Ty
tyFromPat (PVar (Id _ t)) = t 
tyFromPat (PCon (Id _ t) _) = t 
tyFromPat (PLit l) = tyFromLit l 

tyFromLit :: Literal -> Ty 
tyFromLit (IntLit _) = word 
tyFromLit (StrLit _) = string 

pairCon :: Name 
pairCon = Name "pair"

tyFromExp :: Exp Id -> Ty 
tyFromExp (Var (Id _ t)) = t 
tyFromExp (Con (Id _ t) _) = t 
tyFromExp (FieldAccess _ (Id _ t)) = t 
tyFromExp (Lit l) = tyFromLit l 
tyFromExp (Call _ (Id _ t) _) = t
-- here we assume that we have made lambda lift 
-- and infer types for the function of the lambda.
-- We do not have lambdas here.
tyFromExp (Lam _ _ (Just t)) = t
tyFromExp (TyExp _ t) = t

mkPat :: Ty -> TcM (Pat Name, [Exp Name])
mkPat (TyCon (Name "pair") [t1, t2])
  = do 
      n1 <- freshName 
      p <- freshName
      (p1,e1) <- mkPat t2 
      pure (ppair (PVar n1) p1, (Var n1) : e1)
mkPat t = do 
  n <- freshName 
  pure (PVar n, [Var n])


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

createTypeFunDef :: FunDef Id -> TcM DataTy
createTypeFunDef (FunDef sig bdy)
  = do 
      sig' <- freshSignature sig  
      let (Forall vs _) = schemeFromSig sig' 
      mkUniqueType (sigName sig)

freshSignature :: Signature Id -> TcM (Signature Id)
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

freshParam :: Param Id -> TcM (Param Id)
freshParam p@(Typed _ _) = pure p 
freshParam (Untyped n@(Id _ t)) 
  = pure (Typed n t)

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

mkUniqueType :: Name -> TcM DataTy 
mkUniqueType n
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


