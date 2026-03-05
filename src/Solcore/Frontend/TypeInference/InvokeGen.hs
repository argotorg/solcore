module Solcore.Frontend.TypeInference.InvokeGen where

import Data.List
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Primitives.Primitives

-- generate invoke instances for functions

generateDecls :: (FunDef Id, Scheme) -> TcM (DataTy, Instance Name)
generateDecls (fd, sch) =
  do
    let funname = sigName (funSignature fd)
    info ["!> Generating extra definitions for:", pretty (funSignature fd)]
    udt <- mkUniqueType funname sch
    instd <- createInstance udt fd sch
    pure (udt, instd)

-- creating unique function type

mkUniqueType :: Name -> Scheme -> TcM DataTy
mkUniqueType n sch@(Forall vs _) =
  do
    info ["!> Creating unique type for ", pretty n, " :: ", pretty sch]
    i <- incCounter
    let dn = Name $ "t_" ++ pretty n ++ show i
        c = Constr dn []
        dt = DataTy dn vs [c]
    info ["!>>> Result:", pretty dt]
    addUniqueType n dt
    pure dt

-- creating the invoke instances

createInstance :: DataTy -> FunDef Id -> Scheme -> TcM (Instance Name)
createInstance udt fd sch =
  do
    -- instantiating function type signature
    (qs :=> ty) <- fresh sch
    info [">> Starting the creation of instance for ", pretty $ sigName (funSignature fd), " :: ", pretty sch]
    -- getting invoke type from context
    _ <- (askEnv invoke >>= fresh) `wrapError` fd
    -- getting arguments and return type from signature
    let (args, returnTy) = splitTy ty
        args' = case filter (not . isClosureTy) args of
          [] -> [unit] -- no args / all args are closures
          xs -> xs
        tupleArgTy = tupleTyFromList args'
        dn = dataName udt
        selfTy = TyCon dn (TyVar <$> dataParams udt)
    -- building the invoke function signature
    (selfParam, sn) <- freshParam "self" selfTy
    (argParam, an) <- freshParam "arg" tupleArgTy
    -- pattern variables for self type
    (spvs, svs) <- freshPatData udt
    -- pattern variables for arguments
    (sargs, sarg) <- unzip <$> mapM freshPatArg args'
    let isig = Signature [] qs invokeName [selfParam, argParam] (Just returnTy)
        -- building the match of function body
        discr = epair (Var sn) (Var an)
        fname = sigName (funSignature fd)
        ssargs = take (length args) (svs ++ sarg)
        scall = Return (Call Nothing fname ssargs)
        bdy = Match [discr] [([foldr1 ppair (spvs : sargs)], [scall])]
        ifd = FunDef isig [bdy]
        vs' = bv qs `union` bv [tupleArgTy, returnTy, selfTy] `union` bv ifd
        instd = Instance False vs' qs invokableName [tupleArgTy, returnTy] selfTy [ifd]
    info [">> Generated invokable instance:\n", pretty instd]
    pure instd

freshPatData :: DataTy -> TcM (Pat Name, [Exp Name])
freshPatData (DataTy _ _ ((Constr cn ts) : _))
  | null ts =
      do
        pure (PCon cn [], [])
  | otherwise =
      do
        pn <- freshFromString "self"
        pure (PVar pn, [Var pn])
freshPatData dt =
  error $ "freshPatData: expected at least one constructor, got " ++ show dt

freshPatArg :: Ty -> TcM (Pat Name, Exp Name)
freshPatArg ty@(TyCon pn _) =
  do
    -- First try to expand if it's a synonym
    ty' <- maybeExpandSynonym ty
    case ty' of
      TyCon pn' _ | pn' /= pn -> freshPatArg ty' -- synonym was expanded, recurse
      _ -> do
        ti <- askTypeInfo pn
        case constrNames ti of
          [cn] -> do
            (_ :=> ty1) <- askEnv cn >>= fresh
            let (args, _ret) = splitTy ty1
            if null args
              then do
                n <- freshName
                pure (PVar n, Var n)
              else do
                (ps', es) <- unzip <$> mapM freshPatArg args
                pure (PCon cn ps', Con cn es)
          _ -> do
            n <- freshName
            pure (PVar n, Var n)
freshPatArg (TyVar _) =
  do
    n <- freshName
    pure (PVar n, Var n)
freshPatArg _ =
  do
    n <- freshName
    pure (PVar n, Var n)

fresh :: Scheme -> TcM (Qual Ty)
fresh (Forall _ qt) = pure qt

freshParam :: String -> Ty -> TcM (Param Name, Name)
freshParam s t =
  do
    n <- freshFromString s
    pure (Typed n t, n)

freshFromString :: String -> TcM Name
freshFromString s =
  do
    n <- incCounter
    pure (Name (s ++ show n))

isClosureName :: Name -> Bool
isClosureName n = isPrefixOf "t_closure" (pretty n)

isClosureTy :: Ty -> Bool
isClosureTy (TyCon tn _) =
  isClosureName tn
isClosureTy _ = False

ppair :: Pat Name -> Pat Name -> Pat Name
ppair p1 p2 = PCon (Name "pair") [p1, p2]

anfInstance :: Inst -> Inst
anfInstance inst@(_ :=> InCls _ _ []) = inst
anfInstance inst@(q :=> InCls c t as) = q ++ q' :=> InCls c t bs
  where
    q' = zipWith (:~:) bs as
    bs = map TyVar $ take (length as) freshNames
    tvs = bv inst
    freshNames = filter (not . flip elem tvs) (TVar <$> namePool)
anfInstance inst = inst

isQual :: Name -> Bool
isQual (QualName _ _) = True
isQual _ = False

tyParam :: Param a -> TcM Ty
tyParam (Typed _ t) = pure t
tyParam (Untyped _) = freshTyVar

tyFromData :: DataTy -> Ty
tyFromData (DataTy dn vs _) =
  TyCon dn (TyVar <$> vs)

invoke :: Name
invoke = QualName invokableName "invoke"
