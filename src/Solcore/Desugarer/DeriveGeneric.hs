module Solcore.Desugarer.DeriveGeneric where

import Data.List (nub)
import Data.List.NonEmpty (toList)
import Solcore.Frontend.Syntax

-- Generate Generic instances for data types

deriveGenericTopDecls :: [DataTy] -> [TopDecl Name] -> Either String [TopDecl Name]
deriveGenericTopDecls localData allDecls
  | not (genericClassVisible allDecls) = Right allDecls
  | (n : _) <- conflicts = Left (conflictError n)
  | otherwise = Right (allDecls ++ newInsts ++ storageInsts ++ abiInsts)
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
    -- instances so the data type can live in contract storage: StorageSize (the
    -- field's slot footprint, for offset arithmetic) and storage(T):CanStore(T)
    -- (how a field is loaded/stored). Recursive types are skipped — their slot
    -- footprint is unbounded.
    --
    -- Storability is decided by CanStore, not StorageType. The derived CanStore
    -- decomposes the SOP representation through the structural CanStore instances
    -- in std.StorageGeneric, so each leaf is stored via its own CanStore — a
    -- fixed leaf via storage(word)/…, a dynamic leaf (memory(bytes)) via
    -- storage(bytes). A data type with a dynamic field is therefore genuinely
    -- storable, and a field whose type has no CanStore instance fails precisely
    -- at the storage site, not while deriving an instance nobody uses.
    storageInsts
      | storageClassVisible allDecls =
          concatMap buildStorageInstances [dt | dt <- derivable, not (isRecursiveData dt)]
      | otherwise = []
    -- When std.ABIGeneric is in scope, derive per-type ABIAttribs and ABIDecode
    -- instances so the contract dispatch can carry ADT-typed method parameters
    -- and return values.
    --
    -- ABIDecode needs a concrete instance because its decode returns a
    -- result-position type variable a default instance can't monomorphize.
    --
    -- ABIAttribs needs one for a different reason: std declares a catch-all
    -- `default instance t:ABIAttribs` with headSize 32, which outranks the
    -- Generic bridge in std.ABIGeneric. An ADT would then report a 32-byte head
    -- however wide its representation is, truncating returndata to the tag word
    -- and under-checking calldatasize. A concrete instance wins over both
    -- defaults and reports the rep's real head size and staticness.
    --
    -- For a MULTI-constructor type the wire form is variant-tagged: each variant
    -- is discriminated on the wire by a single bytes32 keccak256("Name(argSigs)")
    -- tag (see std.ABIGeneric's encodeVariant / abiSumReader), not
    -- the positional inl/inr 0/1 chain used internally. That tag is per-variant,
    -- and the variant name only survives here (the structural sum representation
    -- has forgotten it), so a concrete, variant-aware ABIEncode AND ABIDecode are
    -- emitted per type — the encoder cannot be the Generic bridge (it would fall
    -- back to the anonymous structural sum's 0/1 tag).
    --
    -- A SINGLE-constructor type has no discriminant (it is a product/struct), so
    -- it keeps the plain bridge: the free ABIEncode bridge and the rep-bridging
    -- ABIDecode, with no tag word added.
    abiInsts
      | abiClassVisible allDecls =
          concatMap abiInstsFor [dt | dt <- derivable, not (isRecursiveData dt)]
      | otherwise = []
    abiInstsFor dt
      | length (dataConstrs dt) >= 2 =
          [ TInstDef (buildABIAttribs dt),
            TInstDef (buildABIEncode dt),
            TInstDef (buildABIDecodeSum dt)
          ]
      | otherwise =
          [TInstDef (buildABIAttribs dt), TInstDef (buildABIDecode dt)]
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

-- The ABIDeriving marker class is declared in std.ABIGeneric; its presence
-- signals that the ABI type classes and their structural instances are in
-- scope, so a per-type ABIDecode instance can be derived.
abiClassVisible :: [TopDecl Name] -> Bool
abiClassVisible = any isMarker
  where
    isMarker (TClassDef cls) = className cls == Name "ABIDeriving"
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
    go (TContr (Contract _ _ _ _ ds)) = [dt | CDataDecl dt <- ds]
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
constrRep (Constr _ [] _) = unitTy
constrRep (Constr _ [t] _) = t
constrRep (Constr _ ts _) = mkProdOf ts

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
fromClause idx total (Constr cname tys _) =
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
toClause idx total (Constr cname tys _) =
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
    TInstDef (buildCanStore dt)
  ]

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
--     CanStore.store(storage(Typedef.rep(r)) : storage(<rep>), Generic.from(v));
--   }
--   function load(r : storage(T(params))) -> T(params) {
--     let x : <rep> = CanStore.load(storage(Typedef.rep(r)) : storage(<rep>));
--     return Generic.to(x);
--   }
-- }
--
-- Storage is expressed entirely through CanStore: the type's SOP representation
-- is stored at the slot via the structural CanStore instances (sum/pair/unit) in
-- std.StorageGeneric, which decompose to each leaf's own CanStore. So a leaf of
-- type memory(bytes) is stored via storage(bytes), and the type stays storable.
-- For parametric types the context carries, per parameter, the field obligations
-- the structural instances need (storage(a):CanStore(a) and a:StorageSize); for a
-- concrete type the context is empty and everything resolves at the leaves.
buildCanStore :: DataTy -> Instance Name
buildCanStore dt =
  Instance
    { instDefault = False,
      instVars = dataParams dt,
      instContext =
        [InCls (Name "CanStore") (storageTyOf (TyVar tv)) [TyVar tv] | tv <- dataParams dt]
          ++ [InCls (Name "StorageSize") (TyVar tv) [] | tv <- dataParams dt],
      instName = Name "CanStore",
      paramsTy = [mainT],
      mainTy = storageTyOf mainT,
      instFunctions = [FunDef False storeSig storeBody, FunDef False loadSig loadBody]
    }
  where
    mainT = mainTyOf dt
    repT = sopRep dt
    -- storage(Typedef.rep(_r)) : storage(<rep>)
    repSlot = TyExp (Con (Name "storage") [methodCall "Typedef" "rep" [Var (Name "_r")]]) (storageTyOf repT)
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
    storeBody = [StmtExp (methodCall "CanStore" "store" [repSlot, methodCall "Generic" "from" [Var (Name "_v")]])]
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
    loadBody =
      [ Let False (Name "_x") (Just repT) (Just (methodCall "CanStore" "load" [repSlot])),
        Return (methodCall "Generic" "to" [Var (Name "_x")])
      ]

boolTy :: Ty
boolTy = TyCon (Name "bool") []

-- instance <ctx> => T(params) : ABIAttribs {
--   function headSize(ty : Proxy(T(params))) -> word {
--     return ABIAttribs.headSize(Proxy : Proxy(<rep>));
--   }
--   function isStatic(ty : Proxy(T(params))) -> bool {
--     return ABIAttribs.isStatic(Proxy : Proxy(<rep>));
--   }
-- }
--
-- Concrete, so it outranks BOTH the catch-all `default instance t:ABIAttribs`
-- in std (headSize = 32) and the Generic bridge in std.ABIGeneric.
buildABIAttribs :: DataTy -> Instance Name
buildABIAttribs dt =
  Instance
    { instDefault = False,
      instVars = dataParams dt,
      instContext = [InCls (Name "ABIAttribs") (TyVar tv) [] | tv <- dataParams dt],
      instName = Name "ABIAttribs",
      paramsTy = [],
      mainTy = mainTyOf dt,
      instFunctions = [FunDef False (sig "headSize" wordTy) (body "headSize"), FunDef False (sig "isStatic" boolTy) (body "isStatic")]
    }
  where
    repT = sopRep dt
    sig method ret =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = Name method,
          sigParams = [Typed False (Name "_ty") (proxyTyOf (mainTyOf dt))],
          sigRetComptime = False,
          sigReturn = Just ret,
          sigPayable = False
        }
    body method = [Return (methodCall "ABIAttribs" method [proxyExpOf repT])]

-- ABIDecoder(ty, reader)
abiDecoderTyOf :: Ty -> Ty -> Ty
abiDecoderTyOf ty reader = TyCon (Name "ABIDecoder") [ty, reader]

-- instance <ctx> => ABIDecoder(T(params), reader) : ABIDecode(T(params)) {
--   function decode(ptr : ABIDecoder(T(params), reader), headOffset : word) -> T(params) {
--     match ptr {
--     | ABIDecoder(rdr) =>
--         let rep_ptr : ABIDecoder(<rep>, reader) = ABIDecoder(rdr);
--         return Generic.to(ABIDecode.decode(rep_ptr, headOffset));
--     }
--   }
-- }
-- Mirrors std.ABIGeneric's free `decode`, but with the data type fixed in the
-- instance head so Generic.to is monomorphizable. The reader is left generic
-- (its WordReader-ness is the only flat obligation); the rep is decoded in the
-- body through the structural ABIDecode instances.
buildABIDecode :: DataTy -> Instance Name
buildABIDecode dt =
  Instance
    { instDefault = False,
      instVars = dataParams dt ++ [readerTv],
      -- Only the reader and, for parametric types, each parameter's decodability
      -- are required. The rep's structural ABIDecode is resolved in the body via
      -- the sum/pair/unit instances (bottoming out at the parameters / concrete
      -- leaves), so we avoid a rep-sized context that would break Patterson.
      instContext =
        InCls (Name "WordReader") readerTy []
          : [InCls (Name "ABIDecode") (abiDecoderTyOf (TyVar tv) readerTy) [TyVar tv] | tv <- dataParams dt],
      instName = Name "ABIDecode",
      paramsTy = [mainT],
      mainTy = abiDecoderTyOf mainT readerTy,
      instFunctions = [FunDef False sig body]
    }
  where
    mainT = mainTyOf dt
    repT = sopRep dt
    readerTv = TVar (Name "_reader")
    readerTy = TyVar readerTv
    sig =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = Name "decode",
          sigParams =
            [ Typed False (Name "_ptr") (abiDecoderTyOf mainT readerTy),
              Typed False (Name "_headOffset") wordTy
            ],
          sigRetComptime = False,
          sigReturn = Just mainT,
          sigPayable = False
        }
    body =
      [ Match
          [Var (Name "_ptr")]
          [ ( [PCon (Name "ABIDecoder") [PVar (Name "_rdr")]],
              [ Let
                  False
                  (Name "_rep_ptr")
                  (Just (abiDecoderTyOf repT readerTy))
                  (Just (Con (Name "ABIDecoder") [Var (Name "_rdr")])),
                Return
                  ( methodCall
                      "Generic"
                      "to"
                      [methodCall "ABIDecode" "decode" [Var (Name "_rep_ptr"), Var (Name "_headOffset")]]
                  )
              ]
            )
          ]
      ]

-- Variant-tagged ABI codec (multi-constructor types only)
--
-- These emit the on-wire discriminant as a single bytes32 keccak256("Name(args)")
-- tag per variant (the ABI form), rather than routing through the Generic bridge,
-- which would fall back to the anonymous structural sum's positional inl/inr 0/1
-- chain. The tricky static/dynamic framing lives in the std.ABIGeneric helpers
-- (encodeVariant / abiSumReader); the derived code computes each variant's tag
-- inline with `tagE` (keccakLit of the name + field SigString, comptime-folded)
-- and supplies the field product plus the branch injection back into the SOP rep.

-- Bare (unqualified) constructor name — the `Name` part of the on-wire signature.
baseName :: Name -> String
baseName (Name s) = s
baseName (QualName _ s) = s

wordLitE :: Integer -> Exp Name
wordLitE = Lit . IntLit

strLitE :: String -> Exp Name
strLitE = Lit . StrLit

-- a + b (also string concat: `Add.add` dispatches on the operand type, so this
-- serves both the word arithmetic below and the tag-string concatenation), a == b,
-- maxWord(a, b), ABIAttribs.headSize(Proxy(t)), SigString.sigStr(Proxy(t)).
addE :: Exp Name -> Exp Name -> Exp Name
addE a b = Call Nothing (QualName (Name "Add") "add") [a, b]

eqE :: Exp Name -> Exp Name -> Exp Name
eqE a b = Call Nothing (QualName (Name "Eq") "eq") [a, b]

maxWordE :: Exp Name -> Exp Name -> Exp Name
maxWordE a b = Call Nothing (Name "maxWord") [a, b]

headSizeE :: Ty -> Exp Name
headSizeE t = methodCall "ABIAttribs" "headSize" [proxyExpOf t]

-- The free `sigStr` from std.dispatch, matching Selector.compute's exact call so
-- the tag stays on the same comptime-folded path.
sigStrE :: Ty -> Exp Name
sigStrE t = Call Nothing (Name "sigStr") [proxyExpOf t]

-- keccak256("Name(argSigs)") for a variant, built inline exactly like
-- Selector.compute (std.dispatch): a string-literal constructor name spliced with
-- the field product's SigString, fed to keccakLit so it folds at comptime to a
-- constant word. `rep` is the variant's constrRep (the () / leaf / product of its
-- fields), whose SigString renders "" / the field sig / the comma-joined fields.
tagE :: Name -> Ty -> Exp Name
tagE cname rep =
  Call
    Nothing
    (Name "keccakLit")
    [addE (addE (addE (strLitE (baseName cname)) (strLitE "(")) (sigStrE rep)) (strLitE ")")]

-- instance <ctx> => T(params) : ABIEncode {
--   function encodeInto(_x, _basePtr, _offset, _tail) -> word {
--     let _isStatic = ABIAttribs.isStatic(Proxy(T(params)));
--     let _innerHead = 32 + maxWord(headSize(rep0), maxWord(headSize(rep1), ...));
--     match _x {
--       | Ci(vs..) => return encodeVariant(keccakLit("Ci(...)"), (vs..), _isStatic, _innerHead, _basePtr, _offset, _tail);
--     }
--   }
-- }
-- Concrete, so it outranks the `default instance a:ABIEncode` Generic bridge.
buildABIEncode :: DataTy -> Instance Name
buildABIEncode dt =
  Instance
    { instDefault = False,
      instVars = dataParams dt,
      instContext =
        concat
          [ [ InCls (Name "ABIAttribs") (TyVar tv) [],
              InCls (Name "ABIEncode") (TyVar tv) [],
              InCls (Name "SigString") (TyVar tv) []
            ]
          | tv <- dataParams dt
          ],
      instName = Name "ABIEncode",
      paramsTy = [],
      mainTy = mainT,
      instFunctions = [FunDef False sig body]
    }
  where
    mainT = mainTyOf dt
    constrs = dataConstrs dt
    reps = map constrRep constrs
    sig =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = Name "encodeInto",
          sigParams =
            [ Typed False (Name "_x") mainT,
              Typed False (Name "_basePtr") wordTy,
              Typed False (Name "_offset") wordTy,
              Typed False (Name "_tail") wordTy
            ],
          sigRetComptime = False,
          sigReturn = Just wordTy,
          sigPayable = False
        }
    -- 32 (tag) + widest branch head — the dynamic sum's inline body footprint.
    innerHeadE = addE (wordLitE 32) (foldr1 maxWordE (map headSizeE reps))
    body =
      [ Let False (Name "_isStatic") (Just boolTy) (Just (methodCall "ABIAttribs" "isStatic" [proxyExpOf mainT])),
        Let False (Name "_innerHead") (Just wordTy) (Just innerHeadE),
        Match [Var (Name "_x")] (map encClause constrs)
      ]
    encClause con@(Constr cname tys _) =
      let vars = freshVarNames (length tys)
          pat = PCon cname (map PVar vars)
          prodE = mkProdExp (map Var vars)
          call =
            Call
              Nothing
              (Name "encodeVariant")
              [ tagE cname (constrRep con),
                prodE,
                Var (Name "_isStatic"),
                Var (Name "_innerHead"),
                Var (Name "_basePtr"),
                Var (Name "_offset"),
                Var (Name "_tail")
              ]
       in ([pat], [Return call])

-- instance <ctx> => ABIDecoder(T(params), reader) : ABIDecode(T(params)) {
--   function decode(_ptr, _headOffset) -> T(params) {
--     match _ptr {
--     | ABIDecoder(_rdr) =>
--         let _isStatic = ABIAttribs.isStatic(Proxy(T(params)));
--         let _sumRdr : reader = abiSumReader(_rdr, _headOffset, _isStatic);
--         let _tag = WordReader.read(_sumRdr);
--         match (_tag == keccakLit("C0(...)")) {
--         | true  => let _dec0 : ABIDecoder(rep0, reader) = ABIDecoder(_sumRdr);
--                    return Generic.to(inl(ABIDecode.decode(_dec0, 32)));
--         | false => ... (last variant is the default branch, no tag check) ...
--         }
--     }
--   }
-- }
buildABIDecodeSum :: DataTy -> Instance Name
buildABIDecodeSum dt =
  Instance
    { instVars = dataParams dt ++ [readerTv],
      instDefault = False,
      instContext =
        InCls (Name "WordReader") readerTy []
          : concat
            [ [ InCls (Name "ABIDecode") (abiDecoderTyOf (TyVar tv) readerTy) [TyVar tv],
                InCls (Name "SigString") (TyVar tv) [],
                InCls (Name "ABIAttribs") (TyVar tv) []
              ]
            | tv <- dataParams dt
            ],
      instName = Name "ABIDecode",
      paramsTy = [mainT],
      mainTy = abiDecoderTyOf mainT readerTy,
      instFunctions = [FunDef False sig body]
    }
  where
    mainT = mainTyOf dt
    readerTv = TVar (Name "_reader")
    readerTy = TyVar readerTv
    constrs = dataConstrs dt
    total = length constrs
    reps = map constrRep constrs
    sig =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = Name "decode",
          sigParams =
            [ Typed False (Name "_ptr") (abiDecoderTyOf mainT readerTy),
              Typed False (Name "_headOffset") wordTy
            ],
          sigRetComptime = False,
          sigReturn = Just mainT,
          sigPayable = False
        }
    -- Build the value of variant `idx`: rebase a decoder on the sum body, decode
    -- the branch product at +32, inject it into the SOP rep at the right position
    -- and map it back to the user type with Generic.to.
    construct idx rep =
      let decName = Name ("_dec" ++ show idx)
       in [ Let False decName (Just (abiDecoderTyOf rep readerTy)) (Just (Con (Name "ABIDecoder") [Var (Name "_sumRdr")])),
            Return
              ( methodCall
                  "Generic"
                  "to"
                  [wrapSumExp idx total (methodCall "ABIDecode" "decode" [Var decName, wordLitE 32])]
              )
          ]
    dispatch [(idx, _cname, rep)] = construct idx rep
    dispatch ((idx, cname, rep) : rest) =
      [ Match
          [eqE (Var (Name "_tag")) (tagE cname rep)]
          [ ([PCon (Name "true") []], construct idx rep),
            ([PCon (Name "false") []], dispatch rest)
          ]
      ]
    dispatch [] = error "buildABIDecodeSum: no constructors"
    body =
      [ Match
          [Var (Name "_ptr")]
          [ ( [PCon (Name "ABIDecoder") [PVar (Name "_rdr")]],
              [ Let False (Name "_isStatic") (Just boolTy) (Just (methodCall "ABIAttribs" "isStatic" [proxyExpOf mainT])),
                Let
                  False
                  (Name "_sumRdr")
                  (Just readerTy)
                  (Just (Call Nothing (Name "abiSumReader") [Var (Name "_rdr"), Var (Name "_headOffset"), Var (Name "_isStatic")])),
                Let False (Name "_tag") (Just wordTy) (Just (methodCall "WordReader" "read" [Var (Name "_sumRdr")]))
              ]
                ++ dispatch (zip3 [0 ..] (map constrName constrs) reps)
            )
          ]
      ]
