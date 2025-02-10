module Solcore.Frontend.TypeInference.InvokeGen where 

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
generateDecls (fd, sch) 
  = do
      let funname = sigName (funSignature fd)
      udt <- mkUniqueType funname sch
      instd <- createInstance udt fd sch
      pure (udt, instd) 

-- creating unique function type 

mkUniqueType :: Name -> Scheme -> TcM DataTy 
mkUniqueType n sch@(Forall vs _)  
  = do
      info ["!> Creating unique type for ", pretty n, " :: ", pretty sch]
      i <- incCounter
      let dn = Name $ "t_" ++ pretty n ++ show i 
          tc = TyCon dn (TyVar <$> vs)
          c = Constr dn []
          dt = DataTy dn vs [c]
      info ["!>>> Result:", pretty dt]
      addUniqueType n dt 
      pure dt 

-- creating the invoke instances 

createInstance :: DataTy -> FunDef Id -> Scheme -> TcM (Instance Name)
createInstance udt fd sch 
  = do
      -- instantiating function type signature 
      qt@(qs :=> ty) <- freshInst sch 
      -- getting invoke type from context 
      (qs' :=> ty') <- askEnv invoke >>= freshInst 
      -- getting arguments and return type from signature 
      let (args, retTy) = splitTy ty
          args' = if null args then [unit] else filter (not . isClosureTy) args
          vunreach = fv qt \\ fv ty 
          argTy = tupleTyFromList args'
          argvars = fv qt 
          dn = dataName udt
          selfTy = TyCon dn (TyVar <$> fv ty `union` fv qs)
      -- building the invoke function signature 
      (selfParam, sn) <- freshParam "self" selfTy
      (argParam, an) <- freshParam "arg" argTy
      -- pattern variables for self type 
      (spvs, svs) <- freshPatData udt  
      -- pattern variables for arguments
      (sargs, sarg) <- unzip <$> mapM freshPatArg args'
      let
        isig = Signature argvars qs invokeName [selfParam, argParam] (Just retTy)
      -- building the match of function body
        nargs = length args 
        discr = epair (Var sn) (Var an)
        fname = sigName (funSignature fd)
        ssargs = take (length args) (svs ++ sarg) 
        scall = Return (Call Nothing fname ssargs)
        bdy = Match [discr] [([foldr1 ppair (spvs : sargs)], [scall])] 
        ifd = FunDef isig [bdy]
        instd = Instance qs invokableName [argTy, retTy] selfTy [ifd]
      info [">> Generated invokable instance:\n", pretty instd]
      pure instd 

freshPatData :: DataTy -> TcM (Pat Name, [Exp Name]) 
freshPatData (DataTy dn vs ((Constr cn ts) : _))
  | null ts 
    = do 
        pure (PCon cn [], [])
  | otherwise
    = do 
        pn <- freshFromString "self" 
        pure (PVar pn, [Var pn])

freshPatArg :: Ty -> TcM (Pat Name, Exp Name)
freshPatArg (TyCon pn ts)  
   = do
        ti <- askTypeInfo pn
        case constrNames ti of 
           [cn] -> do 
                      (ps :=> ty) <- askEnv cn >>= freshInst 
                      let (args, ret) = splitTy ty 
                      (ps, es) <- unzip <$> mapM freshPatArg args 
                      pure (PCon cn ps, Con cn es) 
           _ -> do 
                  n <- freshName
                  pure (PVar n, Var n) 
freshPatArg (TyVar v) 
  = do 
      n <- freshName 
      pure (PVar n, Var n)

freshParam :: String -> Ty -> TcM (Param Name, Name)  
freshParam s t 
  = do 
      n <- freshFromString s 
      pure (Typed n t, n)

freshFromString :: String -> TcM Name 
freshFromString s 
  = do 
      n <- incCounter
      pure (Name (s ++ show n))

removeClosureTy :: [Ty] -> [Ty] 
removeClosureTy tys@(TyCon n _ : ts) 
  | isClosureName n = ts 
  | otherwise = tys 
removeClosureTy tys = tys

isClosureName :: Name -> Bool 
isClosureName n = isPrefixOf "t_closure" (pretty n)

isClosureTy :: Ty -> Bool
isClosureTy (TyCon tn _) 
  = isClosureName tn 
isClosureTy _ = False 

ppair :: Pat Name -> Pat Name -> Pat Name 
ppair p1 p2 = PCon (Name "pair") [p1, p2] 

tupleTyFromList :: [Ty] -> Ty
tupleTyFromList [] = unit 
tupleTyFromList [t] = t 
tupleTyFromList [t1,t2] = pair t1 t2 
tupleTyFromList (t1 : ts) = pair t1 (tupleTyFromList ts)

anfInstance :: Inst -> Inst
anfInstance inst@(q :=> p@(InCls c t [])) = inst
anfInstance inst@(q :=> p@(InCls c t as)) = q ++ q' :=> InCls c t bs 
  where
    q' = zipWith (:~:) bs as
    bs = map TyVar $ take (length as) freshNames
    tvs = fv inst
    freshNames = filter (not . flip elem tvs) (flip TVar False <$> namePool)

isQual :: Name -> Bool
isQual (QualName _ _) = True 
isQual _ = False

tyParam :: Param a -> TcM Ty 
tyParam (Typed _ t) = pure t 
tyParam (Untyped _) = freshTyVar


tyFromData :: DataTy -> Ty 
tyFromData (DataTy dn vs _) 
  = TyCon dn (TyVar <$> vs)

invoke :: Name 
invoke = QualName invokableName "invoke"
