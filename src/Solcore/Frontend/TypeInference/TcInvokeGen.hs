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

createInvokeSig :: DataTy -> Signature Name -> TcM (Signature Name, Id)
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

mkParamForSig :: Ty -> Ty -> TcM ([Param Name], Id)
mkParamForSig argTy selfTy 
  = do 
      let selfArg = Typed (Name "self") selfTy 
      paramName <- freshName 
      pure ([selfArg, Typed paramName argTy], Id paramName argTy)

-- creating invoke body 

-- no pattern matching needed: just make a call to the function 
-- pattern matching needed:
-- * No closure: function receives more than one arguments, which are passed as a tuple 
-- * Closure: pattern match on closure to get the parameters and make the call with arguments.

createInvokeBody :: DataTy -> Signature Name -> Id -> TcM (Body Name)
createInvokeBody (DataTy dt vs [Constr c1 targs]) sig (Id pid ty)
  = do
      let patTys = tyConArgs ty
          cname = sigName sig
      argTys <- mapM tyParam (sigParams sig)
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
        cps <- mapM (\ _ -> PVar <$> freshName) targs 
        -- matching on closure needed
        let n1 = Name "self"
            cp = Typed n1 tc
            cpat = PCon c1 cps
            pats1 = if null pats then [PVar pid] else pats 
            ns1 = if null ns then [Var n1, Var pid] else Var n1 : (Var pid) : (concat ns) 
            pats' = foldr1 ppair (cpat : pats1) 
            ret = Return $ Call Nothing cname ns1
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
    where 
      pairCon = Name "pair" 
      
epair :: Exp Name -> Exp Name -> Exp Name 
epair e1 e2 = Con (Name "pair") [e1, e2]

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


