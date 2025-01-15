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
      udt <- lookupUniqueType fd sch
      instd <- createInstance udt fd sch
      pure (udt, instd) 

-- looking up the unique function type or generate a unique 
-- type for closures. 

lookupUniqueType :: FunDef Id -> Scheme -> TcM DataTy 
lookupUniqueType fd sch 
  = do 
      let funname = sigName (funSignature fd)
      udt <- fromJust <$> lookupUniqueTy funname
      checkUniqueType funname udt sch  

checkUniqueType :: Name -> DataTy -> Scheme -> TcM DataTy
checkUniqueType fn (DataTy dn dvs [c]) (Forall vs qt) 
   = do
        let (vs0, vs1) = splitAt (length dvs) vs
            s = Subst (zip dvs (map TyVar vs0)) 
            udt' = DataTy dn vs [apply s c]
        addUniqueType fn udt' 
        pure udt'
checkUniqueType fn (DataTy dn dvs cs) (Forall vs qt) 
   = do 
        let (vs0, vs1) = splitAt (length dvs) vs 
            s = Subst (zip dvs (map TyVar vs0)) 
            udt' = DataTy dn vs (apply s cs)
        addUniqueType fn udt' 
        pure udt'

-- creating the invoke instances 

createInstance :: DataTy -> FunDef Id -> Scheme -> TcM (Instance Name)
createInstance udt fd sch 
  = do
      -- instantiating function type signature 
      (qs :=> ty) <- freshInst sch 
      -- getting invoke type from context 
      (qs' :=> ty') <- askEnv invoke >>= freshInst 
      -- getting arguments and return type from signature 
      let (args, retTy) = splitTy ty
          args' = if null args then [unit] else filter (not . isClosureTy) args 
          argTy = tupleTyFromList args'
          argvars = union (union (fv argTy) (fv qs)) (fv retTy) 
          selfTy = TyCon (dataName udt) (TyVar <$> argvars)
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
      pure (Instance qs invokableName [argTy, retTy] selfTy [ifd]) 

freshPatData :: DataTy -> TcM (Pat Name, [Exp Name]) 
freshPatData (DataTy dn vs ((Constr cn ts) : _))
  = do 
      ps <- mapM (const freshName) ts 
      pure (PCon cn (map PVar ps), map Var ps)

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

isPair :: Name -> Bool 
isPair (Name n) = n == "pair" 

isWord :: Name -> Bool 
isWord (Name n) = n == "word"

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
