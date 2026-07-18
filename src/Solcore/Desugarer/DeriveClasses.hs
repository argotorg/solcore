module Solcore.Desugarer.DeriveClasses where

import Data.Generics (everywhere, mkT)
import Solcore.Frontend.Syntax

-- Generate type class instances requested by a `deriving (C, ...)` clause on a
-- data declaration.  For each derived class C the generated instance forwards
-- every method through the type's Generic representation:
--
--   forall a b . a:C, b:C =>
--   instance T(a, b) : C {
--     function m(x : T(a,b), y : T(a,b)) -> r {
--       return C.m(Generic.from(x), Generic.from(y));
--     }
--   }
--
-- The universe instances for C over sum/pair/() resolve the forwarded call, so
-- no per-class compiler support is needed: any single-parameter class with
-- those instances can be a `deriving` target.

deriveClassTopDecls :: [DataTy] -> [TopDecl Name] -> Either String [TopDecl Name]
deriveClassTopDecls localData allDecls =
  do
    instss <- mapM (deriveForData classEnv) localData
    pure (allDecls ++ concat instss)
  where
    classEnv = [cls | TClassDef cls <- allDecls]

deriveForData :: [Class Name] -> DataTy -> Either String [TopDecl Name]
deriveForData classEnv dt =
  mapM (deriveOne classEnv dt) (dataDerivings dt)

deriveOne :: [Class Name] -> DataTy -> Name -> Either String (TopDecl Name)
deriveOne classEnv dt cname =
  case findClass classEnv cname of
    Nothing -> Left (notInScopeError dt cname)
    Just cls
      | not (null (paramsVar cls)) -> Left (multiParamError dt cname)
      | otherwise -> Right (TInstDef (buildDelegationInstance dt cname cls))

findClass :: [Class Name] -> Name -> Maybe (Class Name)
findClass classEnv cname =
  case [cls | cls <- classEnv, leafName (className cls) == leafName cname] of
    (cls : _) -> Just cls
    [] -> Nothing

notInScopeError :: DataTy -> Name -> String
notInScopeError dt cname =
  "cannot derive '"
    ++ show cname
    ++ "' for type '"
    ++ show (dataName dt)
    ++ "': class '"
    ++ show cname
    ++ "' is not in scope"

multiParamError :: DataTy -> Name -> String
multiParamError dt cname =
  "cannot derive '"
    ++ show cname
    ++ "' for type '"
    ++ show (dataName dt)
    ++ "': only single-parameter classes can be derived"

-- instance assembly

buildDelegationInstance :: DataTy -> Name -> Class Name -> Instance Name
buildDelegationInstance dt cname cls =
  Instance
    { instDefault = False,
      instVars = dataParams dt,
      instContext = [InCls cname (TyVar v) [] | v <- dataParams dt],
      instName = cname,
      paramsTy = [],
      mainTy = mainT,
      instFunctions = map (buildMethod cname cls mainT isEmpty) (signatures cls)
    }
  where
    mainT = TyCon (dataName dt) (map TyVar (dataParams dt))
    isEmpty = null (dataConstrs dt)

buildMethod :: Name -> Class Name -> Ty -> Bool -> Signature Name -> FunDef Name
buildMethod cname cls mainT isEmpty sig =
  FunDef
    { funIsPublic = False,
      funSignature = newSig,
      funDefBody = [Return body]
    }
  where
    selfVar = mainVar cls
    selfTy = TyVar selfVar
    substT = everywhere (mkT (substSelf selfVar mainT))
    newSig =
      sig
        { sigVars = [],
          sigContext = [],
          sigParams = map (substParamTy substT) (sigParams sig),
          sigRetComptime = False,
          sigReturn = substT <$> sigReturn sig,
          sigPayable = False
        }
    args = map (mkArg selfTy) (sigParams sig)
    callExp = Call Nothing (QualName cname (leafName (sigName sig))) args
    -- The delegated call returns the Generic representation type.  When the
    -- method returns the self type, wrap the result in
    -- Generic.to to convert the representation back to the user type.  This
    -- mirrors mkArg, which wraps self-typed arguments in Generic.from.
    returnsSelf = sigReturn sig == Just selfTy
    delegated
      | returnsSelf = Call Nothing genericTo [callExp]
      | otherwise = callExp
    -- An empty data type has no values, so this method can never be called.
    -- There is no Generic representation for it (the universe has no empty
    -- type), so instead of delegating through Generic we return absurd, a
    -- value of any type that reverts.
    body
      | isEmpty = Call Nothing absurdName []
      | otherwise = delegated

absurdName :: Name
absurdName = Name "absurd"

-- wrap each argument whose declared type is the class self type in Generic.from

mkArg :: Ty -> Param Name -> Exp Name
mkArg selfTy p
  | paramTy p == Just selfTy = Call Nothing genericFrom [Var (paramName p)]
  | otherwise = Var (paramName p)

genericFrom :: Name
genericFrom = QualName (Name "Generic") "from"

genericTo :: Name
genericTo = QualName (Name "Generic") "to"

substParamTy :: (Ty -> Ty) -> Param Name -> Param Name
substParamTy f (Typed c n t) = Typed c n (f t)
substParamTy _ (Untyped c n) = Untyped c n

paramTy :: Param Name -> Maybe Ty
paramTy (Typed _ _ t) = Just t
paramTy (Untyped _ _) = Nothing

substSelf :: Tyvar -> Ty -> Ty -> Ty
substSelf v repl (TyVar v') | v' == v = repl
substSelf _ _ t = t

leafName :: Name -> String
leafName (Name s) = s
leafName (QualName _ s) = s
