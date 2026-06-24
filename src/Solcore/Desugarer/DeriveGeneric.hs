module Solcore.Desugarer.DeriveGeneric where

import Data.List (nub)
import Data.List.NonEmpty (toList)
import Solcore.Frontend.Syntax

-- Generate Generic instances for data types

deriveGenericTopDecls :: [DataTy] -> [TopDecl Name] -> Either String [TopDecl Name]
deriveGenericTopDecls localData allDecls
  | not (genericClassVisible allDecls) = Right allDecls
  | (n : _) <- conflicts = Left (conflictError n)
  | otherwise = Right (allDecls ++ newInsts ++ storageInsts)
  where
    excluded = pragmaExcluded allDecls
    hasInst = existingGenericTypes allDecls
    derivable =
      [ dt
      | dt <- localData,
        not (null (dataConstrs dt)),
        dataName dt `notElem` excluded,
        dataName dt `notElem` hasInst
      ]
    conflicts =
      [ dataName dt
      | dt <- localData,
        dataName dt `elem` hasInst,
        dataName dt `notElem` excluded
      ]
    newInsts = [TInstDef (buildInstance dt) | dt <- derivable]
    -- When std.StorageGeneric is in scope, also derive the storage type-class
    -- instances (StorageSize / CanStore) so the data type can live in contract
    -- storage. StorageType is provided generically by the StorageGeneric bridge,
    -- so only StorageSize (to give the field layout the right slot count) and a
    -- functional CanStore (so contract field access can infer the stored type)
    -- need to be emitted per type. Recursive types are skipped: their storage
    -- size is unbounded.
    storageInsts
      | storageClassVisible allDecls =
          concatMap buildStorageInstances [dt | dt <- derivable, not (isRecursiveData dt)]
      | otherwise = []
    conflictError n =
      "type '"
        ++ show n
        ++ "' has a manual Generic instance "
        ++ "but no 'pragma no-generic-instance-for "
        ++ show n
        ++ "'; "
        ++ "add the pragma to suppress auto-derivation"

genericClassVisible :: [TopDecl Name] -> Bool
genericClassVisible = any isGenericClass
  where
    isGenericClass (TClassDef cls) = className cls == Name "Generic"
    isGenericClass _ = False

-- The StorageDeriving marker class is declared in std.StorageGeneric; its
-- presence signals that the storage type classes and their primitive
-- (sum/pair/unit) instances are in scope, so storage instances can be derived.
storageClassVisible :: [TopDecl Name] -> Bool
storageClassVisible = any isMarker
  where
    isMarker (TClassDef cls) = className cls == Name "StorageDeriving"
    isMarker _ = False

-- A data type is recursive if one of its constructor fields mentions the type
-- itself (directly). Such types have no fixed storage size, so we do not derive
-- storage instances for them.
isRecursiveData :: DataTy -> Bool
isRecursiveData dt =
  any selfRef (concatMap constrTy (dataConstrs dt))
  where
    selfRef t = dataName dt `elem` tyconNames t

collectDataDefs :: [TopDecl Name] -> [DataTy]
collectDataDefs = concatMap go
  where
    go (TDataDef dt) = [dt]
    go (TContr (Contract _ _ ds)) = [dt | CDataDecl dt <- ds]
    go _ = []

existingGenericTypes :: [TopDecl Name] -> [Name]
existingGenericTypes = concatMap go
  where
    go (TInstDef inst)
      | instName inst == Name "Generic" = [tyConName (mainTy inst)]
    go _ = []
    tyConName (TyCon n _) = n
    tyConName _ = Name ""

pragmaExcluded :: [TopDecl Name] -> [Name]
pragmaExcluded = nub . concatMap go
  where
    go (TPragmaDecl (Pragma NoGenericInstanceFor (DisableFor names))) =
      toList names
    go _ = []

-- SOP representation type

unitTy :: Ty
unitTy = TyCon (Name "()") []

mkProdOf :: [Ty] -> Ty
mkProdOf [] = unitTy
mkProdOf [t] = t
mkProdOf (t : ts) = TyCon (Name "pair") [t, mkProdOf ts]

mkSumOf :: [Ty] -> Ty
mkSumOf [] = unitTy
mkSumOf [t] = t
mkSumOf (t : ts) = TyCon (Name "sum") [t, mkSumOf ts]

constrRep :: Constr -> Ty
constrRep (Constr _ []) = unitTy
constrRep (Constr _ [t]) = t
constrRep (Constr _ ts) = mkProdOf ts

sopRep :: DataTy -> Ty
sopRep dt = mkSumOf (map constrRep (dataConstrs dt))

-- Expression helpers

mkProdExp :: [Exp Name] -> Exp Name
mkProdExp [] = Con (Name "()") []
mkProdExp [e] = e
mkProdExp (e : es) = Con (Name "pair") [e, mkProdExp es]

applyInr :: Int -> Exp Name -> Exp Name
applyInr 0 e = e
applyInr n e = Con (Name "inr") [applyInr (n - 1) e]

wrapSumExp :: Int -> Int -> Exp Name -> Exp Name
wrapSumExp _ 1 inner = inner
wrapSumExp idx total inner
  | idx == total - 1 = applyInr (total - 1) inner
  | otherwise = applyInr idx (Con (Name "inl") [inner])

pairPat :: Pat Name -> Pat Name -> Pat Name
pairPat p1 p2 = PCon (Name "pair") [p1, p2]

mkProdPat :: [Name] -> Pat Name
mkProdPat [] = PCon (Name "()") []
mkProdPat [v] = PVar v
mkProdPat vs = foldr1 pairPat (map PVar vs)

applyPInr :: Int -> Pat Name -> Pat Name
applyPInr 0 p = p
applyPInr n p = PCon (Name "inr") [applyPInr (n - 1) p]

wrapSumPat :: Int -> Int -> Pat Name -> Pat Name
wrapSumPat _ 1 inner = inner
wrapSumPat idx total inner
  | idx == total - 1 = applyPInr (total - 1) inner
  | otherwise = applyPInr idx (PCon (Name "inl") [inner])

freshVarNames :: Int -> [Name]
freshVarNames n = [Name ("_gv" ++ show i) | i <- [0 .. n - 1]]

fromClause :: Int -> Int -> Constr -> Equation Name
fromClause idx total (Constr cname tys) =
  let vars = freshVarNames (length tys)
      pat = PCon cname (map PVar vars)
      prodExp = mkProdExp (map Var vars)
      sumExp = wrapSumExp idx total prodExp
   in ([pat], [Return sumExp])

fromBody :: DataTy -> Body Name
fromBody dt =
  let constrs = dataConstrs dt
      total = length constrs
   in [Match [Var (Name "_x")] (zipWith (\i c -> fromClause i total c) [0 ..] constrs)]

toClause :: Int -> Int -> Constr -> Equation Name
toClause idx total (Constr cname tys) =
  let vars = freshVarNames (length tys)
      prodPat = mkProdPat vars
      sumPat = wrapSumPat idx total prodPat
      conExp = Con cname (map Var vars)
   in ([sumPat], [Return conExp])

toBody :: DataTy -> Body Name
toBody dt =
  let constrs = dataConstrs dt
      total = length constrs
   in [Match [Var (Name "_r")] (zipWith (\i c -> toClause i total c) [0 ..] constrs)]

buildFrom :: DataTy -> FunDef Name
buildFrom dt = FunDef False sig (fromBody dt)
  where
    mainT = TyCon (dataName dt) (map TyVar (dataParams dt))
    repT = sopRep dt
    sig =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = Name "from",
          sigParams = [Typed False (Name "_x") mainT],
          sigRetComptime = False,
          sigReturn = Just repT,
          sigPayable = False
        }

buildTo :: DataTy -> FunDef Name
buildTo dt = FunDef False sig (toBody dt)
  where
    mainT = TyCon (dataName dt) (map TyVar (dataParams dt))
    repT = sopRep dt
    sig =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = Name "to",
          sigParams = [Typed False (Name "_r") repT],
          sigRetComptime = False,
          sigReturn = Just mainT,
          sigPayable = False
        }

buildInstance :: DataTy -> Instance Name
buildInstance dt =
  Instance
    { instDefault = False,
      instVars = dataParams dt,
      instContext = [],
      instName = Name "Generic",
      paramsTy = [sopRep dt],
      mainTy = TyCon (dataName dt) (map TyVar (dataParams dt)),
      instFunctions = [buildFrom dt, buildTo dt]
    }

-- Storage instances (StorageSize + CanStore)

mainTyOf :: DataTy -> Ty
mainTyOf dt = TyCon (dataName dt) (map TyVar (dataParams dt))

wordTy :: Ty
wordTy = TyCon (Name "word") []

storageTyOf :: Ty -> Ty
storageTyOf t = TyCon (Name "storage") [t]

proxyTyOf :: Ty -> Ty
proxyTyOf t = TyCon (Name "Proxy") [t]

proxyExpOf :: Ty -> Exp Name
proxyExpOf t = TyExp (Con (Name "Proxy") []) (proxyTyOf t)

-- A qualified class-method call, e.g. StorageType.store(...).
methodCall :: String -> String -> [Exp Name] -> Exp Name
methodCall cls method args = Call Nothing (QualName (Name cls) method) args

buildStorageInstances :: DataTy -> [TopDecl Name]
buildStorageInstances dt =
  [ TInstDef (buildStorageSize dt),
    TInstDef (buildStorageType dt),
    TInstDef (buildCanStore dt)
  ]

-- instance <ctx> => T(params) : StorageType {
--   function load(ptr : word) -> T(params) {
--     let r : <rep> = StorageType.load(ptr);
--     return Generic.to(r);
--   }
--   function store(ptr : word, value : T(params)) -> () {
--     StorageType.store(ptr, Generic.from(value));
--   }
-- }
-- This is emitted as a concrete instance (not a default) on purpose: a default
-- instance's `load` returns the head variable `a`, which the specializer cannot
-- monomorphize for the Generic.to call (the type is in the result, not the
-- arguments). Fixing `a` in the instance head makes Generic.to resolvable.
buildStorageType :: DataTy -> Instance Name
buildStorageType dt =
  Instance
    { instDefault = False,
      instVars = dataParams dt,
      instContext = [InCls (Name "StorageType") (TyVar tv) [] | tv <- dataParams dt],
      instName = Name "StorageType",
      paramsTy = [],
      mainTy = mainT,
      instFunctions = [FunDef False loadSig loadBody, FunDef False storeSig storeBody]
    }
  where
    mainT = mainTyOf dt
    loadSig =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = Name "load",
          sigParams = [Typed False (Name "_ptr") wordTy],
          sigRetComptime = False,
          sigReturn = Just mainT,
          sigPayable = False
        }
    loadBody =
      [ Let False (Name "_r") (Just (sopRep dt)) (Just (methodCall "StorageType" "load" [Var (Name "_ptr")])),
        Return (methodCall "Generic" "to" [Var (Name "_r")])
      ]
    storeSig =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = Name "store",
          sigParams = [Typed False (Name "_ptr") wordTy, Typed False (Name "_v") mainT],
          sigRetComptime = False,
          sigReturn = Just unitTy,
          sigPayable = False
        }
    storeBody = [StmtExp (methodCall "StorageType" "store" [Var (Name "_ptr"), methodCall "Generic" "from" [Var (Name "_v")]])]

-- instance <ctx> => T(params) : StorageSize {
--   function size(x : Proxy(T(params))) -> word {
--     return StorageSize.size(Proxy : Proxy(<rep>));
--   }
-- }
buildStorageSize :: DataTy -> Instance Name
buildStorageSize dt =
  Instance
    { instDefault = False,
      instVars = dataParams dt,
      instContext = [InCls (Name "StorageSize") (TyVar tv) [] | tv <- dataParams dt],
      instName = Name "StorageSize",
      paramsTy = [],
      mainTy = mainTyOf dt,
      instFunctions = [FunDef False sig body]
    }
  where
    sig =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = Name "size",
          sigParams = [Typed False (Name "_x") (proxyTyOf (mainTyOf dt))],
          sigRetComptime = False,
          sigReturn = Just wordTy,
          sigPayable = False
        }
    body = [Return (methodCall "StorageSize" "size" [proxyExpOf (sopRep dt)])]

-- instance <ctx> => storage(T(params)) : CanStore(T(params)) {
--   function store(r : storage(T(params)), v : T(params)) -> () {
--     StorageType.store(Typedef.rep(r), v);
--   }
--   function load(r : storage(T(params))) -> T(params) {
--     return StorageType.load(Typedef.rep(r));
--   }
-- }
buildCanStore :: DataTy -> Instance Name
buildCanStore dt =
  Instance
    { instDefault = False,
      instVars = dataParams dt,
      instContext = [InCls (Name "StorageType") (TyVar tv) [] | tv <- dataParams dt],
      instName = Name "CanStore",
      paramsTy = [mainT],
      mainTy = storageTyOf mainT,
      instFunctions = [FunDef False storeSig storeBody, FunDef False loadSig loadBody]
    }
  where
    mainT = mainTyOf dt
    slot = methodCall "Typedef" "rep" [Var (Name "_r")]
    storeSig =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = Name "store",
          sigParams =
            [ Typed False (Name "_r") (storageTyOf mainT),
              Typed False (Name "_v") mainT
            ],
          sigRetComptime = False,
          sigReturn = Just unitTy,
          sigPayable = False
        }
    storeBody = [StmtExp (methodCall "StorageType" "store" [slot, Var (Name "_v")])]
    loadSig =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = Name "load",
          sigParams = [Typed False (Name "_r") (storageTyOf mainT)],
          sigRetComptime = False,
          sigReturn = Just mainT,
          sigPayable = False
        }
    loadBody = [Return (methodCall "StorageType" "load" [slot])]
