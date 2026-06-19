{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Solcore.Desugarer.ContractDispatch
-- Description : Implements method dispatch via function selectors in calldata
--
-- Adds a runtime entrypoint to each contract that dispatches to the defined
-- contract methods by examining the first four bytes of calldata and comparing it
-- to the computed function selector for each method. The instances and datatypes
-- used to implement this dispatch can be found in std/dispatch.solc.
module Solcore.Desugarer.ContractDispatch
  ( contractDispatchDesugarer,
    contractDispatchTopDecls,
    nameTypeName,
    publicMethodTypes,
  )
where

import Data.List (mapAccumL)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Language.Yul
import Language.Yul.QuasiQuote
import Solcore.Backend.Mast
import Solcore.Frontend.Syntax
import Solcore.Primitives.Primitives (string, tupleExpFromList, tupleTyFromList, unit, word)

contractDispatchDesugarer :: CompUnit Name -> CompUnit Name
contractDispatchDesugarer (CompUnit ims topdecls) = CompUnit ims (contractDispatchTopDecls topdecls)

contractDispatchTopDecls :: [TopDecl Name] -> [TopDecl Name]
contractDispatchTopDecls topdecls = Set.toList extras <> topdecls'
  where
    (extras, topdecls') = mapAccumL go Set.empty topdecls
    go acc (TContr c)
      | "main" `notElem` functionNames c = (Set.union acc (genNameDecls c), TContr (genMainFn True c))
      | otherwise = (acc, TContr (genMainFn False c))
    go acc v = (acc, v)

hasConstructor :: [ContractDecl Name] -> Bool
hasConstructor = any isConstr
  where
    isConstr (CConstrDecl _) = True
    isConstr _ = False

-- | Special internal name used by the parser for the (optional) `fallback`
-- function defined inside a contract.
fallbackName :: Name
fallbackName = Name "fallback"

isFallback :: FunDef a -> Bool
isFallback fd = sigName (funSignature fd) == fallbackName

functionNames :: Contract a -> [Name]
functionNames = foldr go [] . decls
  where
    go (CFunDecl fd) = (sigName (funSignature fd) :)
    go _ = id

-- | Returns the (at most one) user-defined fallback function for a contract.
findFallback :: Contract a -> Maybe (FunDef a)
findFallback c = listToMaybe [fd | CFunDecl fd <- decls c, isFallback fd]

genNameDecls :: Contract Name -> Set (TopDecl Name)
genNameDecls (Contract cname _ cdecls) = foldl go Set.empty cdecls
  where
    go acc (CFunDecl (FunDef True sig _))
      | sigName sig == fallbackName = acc
      | otherwise =
          let dataTy = mkNameTy cname (sigName sig)
              instDef = mkNameInst dataTy (sigName sig)
           in Set.union (Set.fromList [TDataDef dataTy, TInstDef instDef]) acc
    go acc _ = acc

genMainFn :: Bool -> Contract Name -> Contract Name
genMainFn addMain c@(Contract cname tys cdecls)
  | addMain = Contract cname tys (CFunDecl mainfn : Set.toList cdecls')
  | otherwise = Contract cname tys (Set.toList cdecls')
  where
    cdecls'' = if hasConstructor cdecls then cdecls else cdecls ++ [defaultConstructor]
    cdecls' = Set.unions (map (transformCDecl cname) cdecls'')
    defaultConstructor = CConstrDecl (Constructor {constrParams = [], constrBody = [], constrPayable = False})
    mainfn = FunDef False (Signature [] [] "main" [] False (Just unit) False) body
    body = [StmtExp (Call Nothing (QualName "RunContract" "exec") [cdata])]
    cdata = Con "Contract" [methods, fallback]
    methods = tupleExpFromList (fmap mkMethod (mapMaybe unwrapSigs cdecls))
    fallback = case findFallback c of
      Just (FunDef _ sig _) ->
        Con
          "Fallback"
          [ proxyExp (TyCon (if sigPayable sig then "Payable" else "NonPayable") []),
            proxyExp (tupleTyFromList (mapMaybe getTy (sigParams sig))),
            proxyExp (fromMaybe unit (sigReturn sig)),
            Var fallbackName
          ]
      Nothing ->
        Con
          "Fallback"
          [ proxyExp (TyCon "NonPayable" []),
            proxyExp unit,
            proxyExp unit,
            Var "fallback_default_implementation"
          ]

    mkMethod (Signature _ _ fname fargs _ (Just ret) payable)
      | all isTyped fargs =
          Con
            "Method"
            [ proxyExp (TyCon (nameTypeName cname fname) []),
              proxyExp (TyCon (if payable then "Payable" else "NonPayable") []),
              proxyExp (tupleTyFromList (mapMaybe getTy fargs)),
              proxyExp ret,
              Var fname
            ]
    mkMethod s = error $ "Internal Error: contract methods must be fully typed: " <> show s

transformCDecl :: Name -> ContractDecl Name -> Set (ContractDecl Name)
transformCDecl contractName (CConstrDecl c) = transformConstructor contractName c
transformCDecl _ d = Set.singleton d

transformConstructor :: Name -> Constructor Name -> Set (ContractDecl Name)
transformConstructor contractName cons
  | all isTyped params = Set.fromList [initFun, copyArgsFun, startFun]
  | otherwise = error $ "Internal Error: contract constructor must be fully typed"
  where
    params = constrParams cons
    payable = constrPayable cons
    argsTuple = (tupleTyFromList (mapMaybe getTy params))
    initFun = CFunDecl (FunDef False initSig (constrBody cons))
    initSig =
      Signature
        { sigVars = mempty,
          sigContext = mempty,
          sigName = initFunName,
          sigParams = params,
          sigRetComptime = False,
          sigReturn = Just unit,
          sigPayable = False
        }

    copySig =
      Signature
        { sigVars = mempty,
          sigContext = mempty,
          sigName = "copy_arguments_for_constructor",
          sigParams = mempty,
          sigRetComptime = False,
          sigReturn = Just argsTuple,
          sigPayable = False
        }
    contractString = show contractName
    yulContractName = YLit $ YulString contractString
    deployer = YLit $ YulString $ contractString <> "Deploy"
    copyBody
      | null params = [Return (Con "()" [])]
      | otherwise =
          [ Let False "res" (Just argsTuple) Nothing,
            Let False "memoryDataOffset" (Just word) Nothing,
            Asm
              [yulBlock|{
                 let programSize := datasize(`deployer`)
                 let argSize := sub(codesize(), programSize)
                 memoryDataOffset := mload(64)
                 mstore(64, add(memoryDataOffset, argSize))
                 codecopy(memoryDataOffset, programSize, argSize)
              }|],
            Let False "source" (Just (memoryT bytesT)) (Just (memoryE (Var "memoryDataOffset"))),
            Var "res"
              := Call
                Nothing
                "abi_decode"
                [ Var "source",
                  proxyExp argsTuple,
                  proxyExp (TyCon "MemoryWordReader" [])
                ],
            Return (Var "res")
          ]
    memoryT t = TyCon "memory" [t]
    memoryE e = Con "memory" [e]
    bytesT = TyCon "bytes" []
    copyArgsFun = CFunDecl (FunDef False copySig copyBody)

    startSig =
      Signature
        { sigVars = mempty,
          sigContext = mempty,
          sigName = deployerName,
          sigParams = mempty,
          sigRetComptime = False,
          sigReturn = Just unit,
          sigPayable = False
        }
    -- A non-payable constructor must reject any incoming value transfer
    -- during deployment. A payable constructor skips this check. This mirrors
    -- the method-level callvalue check used by the runtime dispatch and
    -- reverts with the same NonPayableReceivedValue error (0xb5988ea3).
    callvalueCheck
      | payable = []
      | otherwise =
          [ StmtExp $
              Call
                Nothing
                (QualName "MethodLevelCallvalueCheck" "checkCallvalue")
                [proxyExp (TyCon "NonPayable" [])]
          ]
    startBody =
      [ Asm [yulBlock|{ mstore(64, memoryguard(128)) }|]
      ]
        <> callvalueCheck
        <> [ Let False "conargs" (Just argsTuple) (Just (Call Nothing "copy_arguments_for_constructor" [])),
             -- , Match [Var "conargs"] ...
             Let False "fun" Nothing (Just (Var initFunName)),
             StmtExp $ Call Nothing "fun" [Var "conargs"],
             Asm
               [yulBlock|{
            let size := datasize(`yulContractName`)
            codecopy(0, dataoffset(`yulContractName`), datasize(`yulContractName`))
            return(0, size)
          }|]
           ]
    startFun = CFunDecl (FunDef False startSig startBody)

initFunName :: Name
initFunName = "init_"

mkNameTy :: Name -> Name -> DataTy
mkNameTy cname fname = DataTy (nameTypeName cname fname) [] []

mkNameInst :: DataTy -> Name -> Instance Name
mkNameInst (DataTy dname [] []) fname =
  let nameTy = TyCon dname []
      sig = Signature [] [] "sigStr" [Typed False "p" (proxyTy nameTy)] False (Just string) False
      body = [Return (Lit (StrLit (show fname)))]
   in Instance
        { instDefault = False,
          instVars = [],
          instContext = [],
          instName = "SigString",
          paramsTy = [],
          mainTy = nameTy,
          instFunctions = [FunDef False sig body]
        }
mkNameInst dt _ = error ("Internal Error: unexpected name type structure: " <> show dt)

-- | The 'Method' type (as used by the dispatcher) for each public,
-- fully-typed method of a contract, in dispatch order.  Used by the
-- @type(C).publicMethods@ primitive to compute interface ids: each 'Method'
-- type has a 'Selector' instance (which reuses 'sigStr'), so the selectors can
-- be derived from these types without reimplementing any hashing in the
-- compiler.  The payability and return types are carried faithfully; the
-- function ('fn') field is irrelevant to the selector and is filled with a
-- 'word' placeholder.  The fallback and any non-fully-typed methods are
-- skipped.
publicMethodTypes :: Contract Name -> [Ty]
publicMethodTypes (Contract cname _ cdecls) =
  mapMaybe methodTy (mapMaybe unwrapSigs cdecls)
  where
    methodTy (Signature _ _ fname fargs _ (Just ret) payable)
      | all isTyped fargs =
          Just $
            TyCon
              "Method"
              [ TyCon (nameTypeName cname fname) [],
                TyCon (if payable then "Payable" else "NonPayable") [],
                tupleTyFromList (mapMaybe getTy fargs),
                ret,
                word
              ]
    methodTy _ = Nothing

--- Util ---

-- | Pull the signature out of a public contract method, skipping the optional
-- fallback function and any non-public ('FunDef False') declarations.  Used
-- both by 'genMainFn' (to build the dispatch table) and 'publicMethodTypes'
-- (to expose the same list as types for the @publicMethods@ primitive).
unwrapSigs :: ContractDecl Name -> Maybe (Signature Name)
unwrapSigs (CFunDecl (FunDef True s _))
  | sigName s == fallbackName = Nothing
  | otherwise = Just s
unwrapSigs _ = Nothing

isTyped :: Param a -> Bool
isTyped (Typed {}) = True
isTyped (Untyped {}) = False

getTy :: Param a -> Maybe Ty
getTy (Typed _ _ t) = Just t
getTy (Untyped {}) = Nothing


proxyTy :: Ty -> Ty
proxyTy t = TyCon "Proxy" [t]

proxyExp :: Ty -> Exp Name
proxyExp t = TyExp (Con "Proxy" []) (proxyTy t)

-- | Generate the name for the name type from the contract and method names
nameTypeName :: Name -> Name -> Name
nameTypeName cname fname = Name ("DispatchNameTy_" <> nm cname <> "_" <> nm fname)
  where
    nm (Name s) = s
    nm (QualName _ s) = s
