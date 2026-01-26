{-# LANGUAGE QuasiQuotes #-}
{-|
Module      : Solcore.Desugarer.ContractDispatch
Description : Implements method dispatch via function selectors in calldata

Adds a runtime entrypoint to each contract that dispatches to the defined
contract methods by examining the first four bytes of calldata and comparing it
to the computed function selector for each method. The instances and datatypes
used to implement this dispatch can be found in std/dispatch.solc.
-}
module Solcore.Desugarer.ContractDispatch where

import Data.Bits ((.|.), shiftL)
import Data.ByteString qualified as BS
import Data.ByteString (ByteString)
import Data.List (mapAccumL)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text qualified as T

import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Primitives.Primitives (word, unit, tupleTyFromList)
import Data.Text.Encoding (encodeUtf8)
import Language.Yul
import Language.Yul.QuasiQuote

contractDispatchDesugarer :: CompUnit Id -> CompUnit Id
contractDispatchDesugarer (CompUnit ims topdecls) = CompUnit ims (Set.toList extras <> topdecls')
  where
    (extras, topdecls') = mapAccumL go Set.empty topdecls
    go acc (TContr c)
      | "main" `notElem` functionNames c = (Set.union acc (genNameDecls c), TContr (genMainFn True c))
      |  otherwise = (acc, TContr (genMainFn False c))
    go acc v = (acc, v)

functionNames :: Contract a -> [Name]
functionNames = foldr go [] . decls where
  go (CFunDecl fd) = (sigName (funSignature fd) :)
  go _ = id

genNameDecls :: Contract Id -> Set (TopDecl Id)
genNameDecls (Contract cname _ cdecls) = foldl go Set.empty cdecls
  where
    go acc (CFunDecl (FunDef sig _)) = let
        dataTy = mkNameTy cname (sigName sig)
        instDef = mkNameInst dataTy (sigName sig)
      in Set.union (Set.fromList [TDataDef dataTy, TInstDef instDef]) acc
    go acc _ = acc

genMainFn :: Bool -> Contract Id -> Contract Id
genMainFn addMain (Contract cname tys cdecls)
  | addMain = Contract cname tys (CFunDecl mainfn : Set.toList cdecls')
  | otherwise = Contract cname tys (Set.toList cdecls')
  where
    cdecls' = Set.unions (map (transformCDecl cname) cdecls)
    mainfn :: FunDef Id
    mainfn = FunDef (Signature [] [] "main" [] Nothing) body
    body = [ StmtExp (Call Nothing (Id (QualName "RunContract" "exec") someType) [cdata])]
    cdata = Con (Id "Contract" someType) [methods, fallback]
    methods = tupleExpFromList (fmap mkMethod (mapMaybe unwrapSigs cdecls))
    fallback = Con (Id "Fallback" someType)
      [ proxyExp (TyCon "NonPayable" [])
      , proxyExp unit
      , proxyExp unit
      , Var (Id "revert_handler" someType)
      ]

    mkMethod :: Signature Id -> Exp Id
    mkMethod (Signature _ _ fname fargs (Just ret)) | all isTyped fargs = Con (Id "Method" someType)
      [ proxyExp (TyCon (nameTypeName cname fname) [])
      , proxyExp (TyCon "NonPayable" [])
      , proxyExp (tupleTyFromList (mapMaybe getTy fargs))
      , proxyExp ret
      , (Var (Id fname  someType))
      ]
    mkMethod s = error $ "Internal Error: contract methods must be fully typed: " <> show s

    unwrapSigs (CFunDecl (FunDef s _)) = Just s
    unwrapSigs _ = Nothing

    isTyped (Typed {}) = True
    isTyped (Untyped {}) = False

    getTy (Typed _ t) = Just t
    getTy (Untyped {}) = Nothing

transformCDecl :: Name -> ContractDecl Id -> Set (ContractDecl Id)
transformCDecl contractName (CConstrDecl c) = transformConstructor contractName c
transformCDecl _ d = Set.singleton d

transformConstructor :: Name -> Constructor Id -> Set (ContractDecl Id)
transformConstructor contractName cons
  | all isTyped params = Set.fromList[initFun, copyArgsFun, startFun]
  | otherwise = error $ "Internal Error: contract constructor must be fully typed"
  where
  params = constrParams cons
  argsTuple = (tupleTyFromList (mapMaybe getTy params))
  initFun = CFunDecl (FunDef initSig (constrBody cons))
  initSig = Signature
    { sigVars = mempty
    , sigContext = mempty
    , sigName = initFunName
    , sigParams = params
    , sigReturn = Just unit
    }

  copySig = Signature
    { sigVars = mempty
    , sigContext = mempty
    , sigName = "copy_arguments_for_constructor"
    , sigParams = mempty
    , sigReturn = Just argsTuple
    }
  contractString = show contractName
  yulContractName = YLit $ YulString contractString
  deployer = YLit $ YulString $ contractString <> "Deploy"
  copyBody =
    [ Let (Id "res" word) (Just argsTuple) Nothing
    , Let (Id "memoryDataOffset" word) (Just word) Nothing
    , Asm [yulBlock|{
             let programSize := datasize(`deployer`)
             let argSize := sub(codesize(), programSize)
             memoryDataOffset := mload(64)
             mstore(64, add(memoryDataOffset, argSize))
             codecopy(memoryDataOffset, programSize, argSize)
          }|]
    , Let (Id "source" someType) (Just (memoryT bytesT)) (Just (memoryE(Var (Id "memoryDataOffset" word))))
    , Var (Id "res" word) := Call Nothing (Id "abi_decode" someType)
      [ Var (Id "source" someType)
      , proxyExp argsTuple
      , proxyExp (TyCon "MemoryWordReader" [])
      ]
    , Return (Var (Id "res" word))
    ]
  memoryT t = TyCon "memory" [t]
  memoryE e = Con (Id "memory" someType) [e]
  bytesT = TyCon "bytes" []
  copyArgsFun = CFunDecl (FunDef copySig copyBody)

  startSig :: Signature Id
  startSig = Signature
    { sigVars = mempty
    , sigContext = mempty
    , sigName = "start"
    , sigParams = mempty
    , sigReturn = Just unit
    }
  startBody =
    [ Asm [yulBlock|{ mstore(64, memoryguard(128)) }|]
    , Let (Id "conargs" someType) (Just argsTuple)  (Just (Call Nothing (Id "copy_arguments_for_constructor" someType) []))
    -- , Match [Var "conargs"] ...
    , Let (Id "fun" someType) Nothing (Just (Var (Id initFunName someType)))
    , StmtExp $ Call Nothing (Id "fun" someType) [Var (Id "conargs" someType)]
    , Asm [yulBlock|{
            let size := datasize(`yulContractName`)
            codecopy(0, dataoffset(`yulContractName`), datasize(`yulContractName`))
            return(0, size)
          }|]
    ]
  startFun = CFunDecl (FunDef startSig startBody)

  isTyped (Typed {}) = True
  isTyped (Untyped {}) = False

  getTy (Typed _ t) = Just t
  getTy (Untyped {}) = Nothing

initFunName :: Name
initFunName = "init_"

mkNameTy :: Name -> Name -> DataTy
mkNameTy cname fname = DataTy (nameTypeName cname fname) [] []

mkNameInst :: DataTy -> Name -> Instance Id
mkNameInst (DataTy dname [] []) fname = let
    -- types
    nameTy = TyCon dname []

    -- sig
    typed n t = Typed (Id n t) t
    args = [typed "head" word, typed "tail" word, typed "prx" (proxyTy nameTy)]
    sig = Signature [] [] "append" args (Just word)

    -- body
    nameBytes = encodeUtf8 . T.pack . show $ fname
    nameSize = toInteger (BS.length nameBytes)

    storeSize = mstore (YIdent "head") (add (mload (YIdent "head")) (YLit (YulNumber nameSize)))
    storeBytes = generateStoreBytes nameBytes "tail"
    ret = Return (Call Nothing (Id (QualName "Add" "add") someType) [Var (Id "tail" word), Lit (IntLit nameSize)])
    body :: Body Id
    body = [ Asm (YExp storeSize : storeBytes), ret ]

    -- yul helpers
    mstore loc val = YCall "mstore" [loc, val]
    mload loc = YCall "mload" [loc]
    add l r = YCall "add" [l, r]
  in Instance
    { instDefault = False
    , instVars = []
    , instContext = []
    , instName = "ABIString"
    , paramsTy = []
    , mainTy = TyCon dname []
    , instFunctions = [FunDef sig body]
    }
mkNameInst dt _ = error ("Internal Error: unexpect name type structure: " <> show dt)

--- Util ---
someType :: Ty
someType = TyVar (Skolem "unknownType")

proxyTy :: Ty -> Ty
proxyTy t = TyCon "Proxy" [t]

proxyExp :: Ty -> Exp Id
proxyExp t = TyExp (Con (Id "Proxy" (proxyTy t)) []) (proxyTy t)

-- | Generate the name for the name type from the contract and method names
nameTypeName :: Name -> Name -> Name
nameTypeName cname fname = Name ("DispatchNameTy_" <> nm cname <> "_" <> nm fname)
  where
    nm (Name s) = s
    nm (QualName _ s) = s

-- | Generate Yul statements to store bytes in memory
generateStoreBytes :: ByteString -> Name -> [YulStmt]
generateStoreBytes bs offsetName =
  let chunks = chunk32 bs
      indices = [0, 32 .. (length chunks - 1) * 32]
  in zipWith (storeChunk offsetName) indices chunks

-- | Store a 32-byte chunk at a specific offset
storeChunk :: Name -> Int -> ByteString -> YulStmt
storeChunk offsetName index chunk =
  let paddedChunk = BS.append chunk (BS.replicate (32 - BS.length chunk) 0)
      loc = YCall "add" [YIdent offsetName, YLit (YulNumber (toInteger index))]
      val = YLit (YulNumber (bsToInteger paddedChunk))
  in YExp (YCall "mstore" [loc, val])

-- | Split a ByteString into 32-byte chunks
chunk32 :: ByteString -> [ByteString]
chunk32 bs
  | BS.null bs = []
  | otherwise =
      let (chunk, rest) = BS.splitAt 32 bs
      in chunk : chunk32 rest

-- | Convert a bytestring to an integer (little endian)
bsToInteger :: ByteString -> Integer
bsToInteger = BS.foldr f 0 . BS.reverse
  where
    f w n = toInteger w .|. shiftL n 8

-- Analoguous to identically named function in Primitives, but on Ids
tupleExpFromList :: [Exp Id] -> Exp Id
tupleExpFromList [] = Con (Id "()" unit) []
tupleExpFromList [e] = e
tupleExpFromList [e1,e2] = epair e1 e2
tupleExpFromList (e1 : es) = epair e1 (tupleExpFromList es)

epair :: Exp Id -> Exp Id -> Exp Id
epair e1 e2 = Con (Id "pair" someType) [e1, e2]
