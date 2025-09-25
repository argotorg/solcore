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
import Solcore.Primitives.Primitives (word, unit, tupleExpFromList, tupleTyFromList)
import Data.Text.Encoding (encodeUtf8)
import Language.Yul

contractDispatchDesugarer :: CompUnit Name -> CompUnit Name
contractDispatchDesugarer (CompUnit ims topdecls) = CompUnit ims (Set.toList extras <> topdecls')
  where
    (extras, topdecls') = mapAccumL go Set.empty topdecls

    go acc (TContr c) = (Set.union acc (genNameDecls c), TContr (genMainFn c))
    go acc v = (acc, v)

genNameDecls :: Contract Name -> Set (TopDecl Name)
genNameDecls (Contract cname _ cdecls) = foldl go Set.empty cdecls
  where
    go acc (CFunDecl (FunDef sig _)) = let
        dataTy = mkNameTy cname (sigName sig)
        instDef = mkNameInst dataTy (sigName sig)
      in Set.union (Set.fromList [TDataDef dataTy, TInstDef instDef]) acc
    go acc _ = acc

genMainFn :: Contract Name -> Contract Name
genMainFn (Contract cname tys cdecls) = Contract cname tys ((CFunDecl mainfn) : cdecls)
  where
    mainfn = FunDef (Signature [] [] (Name "main") [] Nothing) body
    body = [ StmtExp (Call Nothing (QualName "RunContract" "exec") [cdata])]
    cdata = Con (Name "Contract") [methods, fallback]
    methods = tupleExpFromList (fmap mkMethod (mapMaybe unwrapSigs cdecls))
    fallback = Con (Name "Fallback")
      [ proxyExp (TyCon (Name "NonPayable") [])
      , proxyExp unit
      , proxyExp unit
      , Var (Name "revert_handler")
      ]

    mkMethod (Signature _ _ fname fargs (Just ret)) | all isTyped fargs = Con (Name "Method")
      [ proxyExp (TyCon (nameTypeName cname fname) [])
      , proxyExp (TyCon (Name "NonPayable") [])
      , proxyExp (tupleTyFromList (mapMaybe getTy fargs))
      , proxyExp ret
      , Var fname
      ]
    mkMethod s = error $ "Internal Error: contract methods must be fully typed: " <> show s

    unwrapSigs (CFunDecl (FunDef s _)) = Just s
    unwrapSigs _ = Nothing

    isTyped (Typed {}) = True
    isTyped (Untyped {}) = False

    getTy (Typed _ t) = Just t
    getTy (Untyped {}) = Nothing


mkNameTy :: Name -> Name -> DataTy
mkNameTy cname fname = DataTy (nameTypeName cname fname) [] []

mkNameInst :: DataTy -> Name -> Instance Name
mkNameInst (DataTy dname [] []) fname = let
    -- types
    nameTy = TyCon dname []

    -- sig
    nhead = Name "head"
    ntail = Name "tail"
    args = [Typed nhead word, Typed ntail word, Typed (Name "prx") (proxyTy nameTy)]
    sig = Signature [] [] (Name "append") args (Just word)

    -- body
    nameBytes = encodeUtf8 . T.pack . show $ fname
    nameSize = toInteger (BS.length nameBytes)

    storeSize = mstore (YIdent nhead) (add (mload (YIdent nhead)) (YLit (YulNumber nameSize)))
    storeBytes = generateStoreBytes nameBytes ntail
    ret = Return (Call Nothing (QualName "Add" "add") [Var ntail, Lit (IntLit nameSize)])
    body = [ Asm (YExp storeSize : storeBytes), ret ]

    -- yul helpers
    mstore loc val = YCall (Name "mstore") [loc, val]
    mload loc = YCall (Name "mload") [loc]
    add l r = YCall (Name "add") [l, r]
  in Instance
    { instDefault = False
    , instVars = []
    , instContext = []
    , instName = Name "ABIString"
    , paramsTy = []
    , mainTy = TyCon dname []
    , instFunctions = [FunDef sig body]
    }
mkNameInst dt _ = error ("Internal Error: unexpect name type structure: " <> show dt)

--- Util ---

proxyTy :: Ty -> Ty
proxyTy t = TyCon (Name "Proxy") [t]

proxyExp :: Ty -> Exp Name
proxyExp t = TyExp (Con (Name "Proxy") []) (proxyTy t)

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
      loc = YCall (Name "add") [YIdent offsetName, YLit (YulNumber (toInteger index))]
      val = YLit (YulNumber (bsToInteger paddedChunk))
  in YExp (YCall (Name "mstore") [loc, val])

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
