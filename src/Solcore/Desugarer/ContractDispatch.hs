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
    writeContractAbis,
    contractAbiJson,
  )
where

import Control.Monad (forM_, unless)
import Data.List (intercalate, mapAccumL)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Language.Yul
import Language.Yul.QuasiQuote
import Solcore.Backend.Mast
import Solcore.Frontend.Syntax
import Solcore.Primitives.Primitives (string, tupleExpFromList, tupleTyFromList, unit, word)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import Text.Printf (printf)

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

    -- skip the optional fallback function and non-public methods in the methods tuple
    unwrapSigs (CFunDecl (FunDef True s _))
      | sigName s == fallbackName = Nothing
      | otherwise = Just s
    unwrapSigs _ = Nothing

    isTyped (Typed {}) = True
    isTyped (Untyped {}) = False

    getTy (Typed _ _ t) = Just t
    getTy (Untyped {}) = Nothing

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
                 // NOTE: we ensure no truncation in startBody below
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
          [ Asm
              [yulBlock|{
                if callvalue() {
                  mstore(0, 0xb5988ea3)
                  revert(28, 4)
                }
              }|]
          ]
    startBody =
      [ Asm
          [yulBlock|{
            mstore(64, memoryguard(128))
            // Guard against truncated deployer (which also covers yulContractName)
            // A truncated input would cause `copy_arguments_for_constructor`
            // to underflow and do an impossible memory expansion resulting in OOG.
            // And such a truncation is unpredictable.
            // TODO: use require with proper error
            if lt(codesize(), datasize(`deployer`)) { revert(0, 0) }
        }|]
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

    isTyped (Typed {}) = True
    isTyped (Untyped {}) = False

    getTy (Typed _ _ t) = Just t
    getTy (Untyped {}) = Nothing

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

--- Util ---

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

nameStr :: Name -> String
nameStr (Name s) = s
nameStr (QualName _ s) = s

--- ABI generation ---

-- | Write a JSON ABI file for every contract among the given declarations.
writeContractAbis :: FilePath -> [TopDecl Name] -> IO ()
writeContractAbis outDir topdecls = do
  let cs = [c | TContr c <- topdecls]
  unless (null cs) (createDirectoryIfMissing True outDir)
  forM_ cs $ \c ->
    writeFile (outDir </> nameStr (name c) <.> "abi") (contractAbiJson c)

-- | Render the JSON ABI description of a contract: an array of constructor,
-- function and fallback descriptors.
contractAbiJson :: Contract Name -> String
contractAbiJson c =
  renderJson (JArr (map abiEntryJson (contractAbiEntries c))) <> "\n"

-- | A single ABI descriptor.
data AbiEntry
  = AbiFunction String [AbiParam] [AbiParam] String
  | AbiConstructor [AbiParam] String
  | AbiFallback String

-- | An input or output entry. Tuple types carry their (recursive) components.
data AbiParam = AbiParam
  { abiParamName :: String,
    abiParamType :: String,
    abiParamComponents :: [AbiParam]
  }

contractAbiEntries :: Contract Name -> [AbiEntry]
contractAbiEntries = mapMaybe entry . decls
  where
    entry (CConstrDecl con) =
      Just (AbiConstructor (map abiParam (constrParams con)) (stateMutability (constrPayable con)))
    entry (CFunDecl (FunDef isPublic sig _))
      | sigName sig == fallbackName = Just (AbiFallback (stateMutability (sigPayable sig)))
      | isPublic =
          Just $
            AbiFunction
              (nameStr (sigName sig))
              (map abiParam (sigParams sig))
              (abiOutputs (sigReturn sig))
              (stateMutability (sigPayable sig))
      | otherwise = Nothing
    entry _ = Nothing

-- | The ABI @stateMutability@ field admits four values: @pure@, @view@,
-- @nonpayable@ and @payable@. This prototype only tracks payability, not whether
-- a function reads or writes state, so we can only distinguish @payable@ from
-- @nonpayable@ and conservatively report the latter for everything else.
stateMutability :: Bool -> String
stateMutability payable = if payable then "payable" else "nonpayable"

abiParam :: Param Name -> AbiParam
abiParam (Typed _ pname t) = mkAbiParam (nameStr pname) t
abiParam (Untyped _ pname) = AbiParam (nameStr pname) "" []

-- | A comma-separated return list @(a, b, c)@ desugars to nested pairs; the ABI
-- represents it as one output per element. A unit return has no outputs.
abiOutputs :: Maybe Ty -> [AbiParam]
abiOutputs Nothing = []
abiOutputs (Just t)
  | t == unit = []
  | otherwise = map (mkAbiParam "") (flattenTuple t)

-- | Flatten a right-nested @pair@ chain into its element list. Because every
-- comma-tuple desugars to right-nested pairs, a flat tuple @(a, b, c)@ and a
-- tuple whose tail is itself a tuple @(a, (b, c))@ share the same
-- representation, so this cannot tell them apart and always flattens fully.
-- That ambiguity is inherent to the language's tuple encoding, not specific to
-- the ABI.
flattenTuple :: Ty -> [Ty]
flattenTuple (TyCon (Name "pair") [a, b]) = a : flattenTuple b
flattenTuple t = [t]

mkAbiParam :: String -> Ty -> AbiParam
mkAbiParam pname t =
  let (tyStr, comps) = abiTypeOf t
   in AbiParam pname tyStr comps

-- | Map a Solcore type to its canonical ABI type name and (for tuples) its
-- component parameters. Memory/calldata are location qualifiers and are
-- transparent to the ABI. The native @word@ maps to @uint256@; the remaining
-- value-type names (uint256, address, bytes32, bool, bytes, string, ...) are
-- nullary type constructors whose names already match the Solidity ABI
-- spelling, so they are passed through unchanged.
abiTypeOf :: Ty -> (String, [AbiParam])
abiTypeOf (TyCon (Name "memory") [t]) = abiTypeOf t
abiTypeOf (TyCon (Name "calldata") [t]) = abiTypeOf t
abiTypeOf t@(TyCon (Name "pair") [_, _]) =
  ("tuple", map (mkAbiParam "") (flattenTuple t))
abiTypeOf (TyCon (Name "word") []) = ("uint256", [])
abiTypeOf (TyCon n []) = (nameStr n, [])
-- Anything else has no ABI spelling: a type variable, a function type, or a
-- parameterized type constructor (e.g. @mapping(word, word)@ or a custom
-- generic) that is not one of the location/tuple cases handled above. Dropping
-- the type arguments here would emit a bare, invalid ABI string like
-- @"type":"mapping"@, which downstream ABI consumers (etherscan, ethers,
-- web3py) would misparse — so fail loudly instead.
abiTypeOf t = error ("contractAbiJson: cannot represent type in ABI: " <> show t)

abiEntryJson :: AbiEntry -> Json
abiEntryJson (AbiFunction fname ins outs mut) =
  JObj
    [ ("inputs", JArr (map abiParamJson ins)),
      ("name", JStr fname),
      ("outputs", JArr (map abiParamJson outs)),
      ("stateMutability", JStr mut),
      ("type", JStr "function")
    ]
abiEntryJson (AbiConstructor ins mut) =
  JObj
    [ ("inputs", JArr (map abiParamJson ins)),
      ("stateMutability", JStr mut),
      ("type", JStr "constructor")
    ]
abiEntryJson (AbiFallback mut) =
  JObj
    [ ("stateMutability", JStr mut),
      ("type", JStr "fallback")
    ]

-- | @internalType@ and @type@ coincide for the value, @string@ and @bytes@
-- types we expose today; solc only diverges them for structs, enums and
-- contract types, which have no surface here yet.
abiParamJson :: AbiParam -> Json
abiParamJson p =
  JObj $
    [ ("internalType", JStr (abiParamType p)),
      ("name", JStr (abiParamName p)),
      ("type", JStr (abiParamType p))
    ]
      <> [("components", JArr (map abiParamJson (abiParamComponents p))) | not (null (abiParamComponents p))]

--- Minimal JSON rendering ---

data Json
  = JStr String
  | JArr [Json]
  | JObj [(String, Json)]

renderJson :: Json -> String
renderJson = go 0
  where
    go _ (JStr s) = jsonString s
    go _ (JArr []) = "[]"
    go ind (JArr xs) =
      "[\n"
        <> intercalate ",\n" [indent (ind + 1) <> go (ind + 1) x | x <- xs]
        <> "\n"
        <> indent ind
        <> "]"
    go _ (JObj []) = "{}"
    go ind (JObj kvs) =
      "{\n"
        <> intercalate ",\n" [indent (ind + 1) <> jsonString k <> ": " <> go (ind + 1) v | (k, v) <- kvs]
        <> "\n"
        <> indent ind
        <> "}"
    indent n = replicate (2 * n) ' '

jsonString :: String -> String
jsonString s = '"' : concatMap esc s <> "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc '\b' = "\\b"
    esc '\f' = "\\f"
    -- JSON requires the remaining control characters (U+0000–U+001F) to be
    -- escaped as \uXXXX. Printable characters (incl. UTF-8) pass through.
    esc c
      | c < '\x20' = printf "\\u%04x" (fromEnum c)
      | otherwise = [c]
