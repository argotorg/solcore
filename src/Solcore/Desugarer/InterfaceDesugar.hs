-- Desugars interface I { function m(args) returns (r); ... } into a typed
-- external-call facade, BEFORE name resolution, so the generated constructor and
-- trait are visible to the rest of the pipeline. For each interface I it emits:
--
--   * a handle newtype   enum I { I(address) }   (+ Typedef/StorageType/CanStore)
--   * an internal trait  trait I_iface<self> { m(s:self, args) -> r; ... }
--   * an instance        impl I_iface<I> { <external-call stub per method> }
--
-- A stub builds selector ++ abi_encode(args) and invokes raw_call, then
-- bubbles the callee's revert on failure or decodes the return on success.
-- I(addr) constructs the handle; x.m(args) resolves through the existing UFCS
-- + instance selection to the stub.
module Solcore.Desugarer.InterfaceDesugar
  ( desugarInterfaces,
  )
where

import Data.List (intercalate)
import Solcore.Frontend.Pretty.TreePretty (pretty)
import Solcore.Frontend.Syntax.Name (Name, pattern Name)
import Solcore.Frontend.Syntax.SyntaxTree

desugarInterfaces :: CompUnit -> Either String CompUnit
desugarInterfaces (CompUnit imps ds) = do
  checkAllImplements ds
  pure (CompUnit imps (concatMap expand ds))
  where
    expand (TContr (ContractShell InterfaceKind n _ ds')) =
      expandInterface (pretty n) [sig | CSignatureDecl _ sig <- ds']
    expand d = [d]

expandInterface :: String -> [Signature] -> [TopDecl]
expandInterface ifn sigs =
  [ TDataDef (DataTy (Name ifn) [] [Constr (Name ifn) [tv "address"]]),
    TInstDef (inst "Typedef" [tv "address"] (tv ifn) [absFun, repFun]),
    TInstDef (inst "StorageType" [] (tv ifn) [stLoad, stStore]),
    TInstDef (inst "CanStore" [tv ifn] (tycon "storage" [tv ifn]) [csStore, csLoad]),
    TClassDef (Class [tv "self"] [] (Name cls) [] (tv "self") (map traitSig sigs)),
    TInstDef (inst cls [] (tv ifn) (map stub sigs))
  ]
  where
    cls = ifn ++ "_iface"

    absFun = fun "abs" [param "a" (tv "address")] (tv ifn) [Return (con ifn [var "a"])]
    repFun =
      fun "rep" [param "x" (tv ifn)] (tv "address") $
        [Match [var "x"] [([Pat (Name ifn) [pvar "a"]], [Return (var "a")])]]

    stLoad = fun "load" [param "ptr" (tv "word")] (tv ifn) [Return (con ifn [callQ "StorageType" "load" [var "ptr"]])]
    stStore =
      fun "store" [param "ptr" (tv "word"), param "value" (tv ifn)] unitTy $
        [StmtExp (callQ "StorageType" "store" [var "ptr", callQ "Typedef" "rep" [var "value"]])]

    csStore =
      fun "store" [param "l" (tycon "storage" [tv ifn]), param "r" (tv ifn)] unitTy $
        [StmtExp (callQ "StorageType" "store" [callQ "Typedef" "rep" [var "l"], var "r"])]
    csLoad = fun "load" [param "l" (tycon "storage" [tv ifn])] (tv ifn) [Return (callQ "StorageType" "load" [callQ "Typedef" "rep" [var "l"]])]

    traitSig sig = sig' (methodName sig) (param "s" (tv "self") : sigParams sig) (retTy sig)

    stub sig = fun (methodName sig) (param "s" (tv ifn) : sigParams sig) (retTy sig) (stubBody sig)

    stubBody sig =
      [ Let False (Name "sel") (Just (tv "word")) (Just (callF "keccakLit" [strLit (canonicalSig sig)])),
        Let False (Name "payload") Nothing (Just (payloadExp sig)),
        Match [rawCall] [([Pat (Name "pair") [pvar "ok", pvar "ret"]], caseBody sig)]
      ]

    rawCall = callF "raw_call" [callQ "Typedef" "rep" [var "s"], callF "uint256" [intLit 0], var "payload"]

    caseBody sig = bubble : decodeReturn sig

    bubble =
      Match
        [var "ok"]
        [ ([Pat (Name "false") []], [StmtExp (callF "revert_" [callQ "MemoryPointer" "ptr" [var "ret"], callQ "MemorySize" "len" [var "ret"]])]),
          ([Pat (Name "true") []], [])
        ]

    decodeReturn sig
      | isVoid sig = []
      | otherwise =
          [ Let False (Name "dat") (Just (tycon "memory" [tv "bytes"])) (Just (callF "memory" [callQ "MemoryPointer" "ptr" [var "ret"]])),
            Return (callF "abi_decode" [var "dat", ExpAt (retTy sig), ExpAt (tv "MemoryWordReader")])
          ]

    payloadExp sig =
      let selRef = callF "truncate" [callF "to_bytes" [callF "bytes32" [var "sel"]], intLit 4]
       in case sigParams sig of
            [] -> selRef
            ps -> callF "concat" [selRef, callF "abi_encode" [tupleExp (map (var . pretty . pName) ps)]]

tv :: String -> Ty
tv s = TyCon (Name s) []

tycon :: String -> [Ty] -> Ty
tycon s = TyCon (Name s)

unitTy :: Ty
unitTy = tv "()"

var :: String -> Exp
var s = ExpVar Nothing (Name s)

con :: String -> [Exp] -> Exp
con s = ExpName Nothing (Name s)

callF :: String -> [Exp] -> Exp
callF s = ExpName Nothing (Name s)

callQ :: String -> String -> [Exp] -> Exp
callQ recv m = ExpName (Just (var recv)) (Name m)

intLit :: Integer -> Exp
intLit n = Lit (IntLit n)

strLit :: String -> Exp
strLit = Lit . StrLit

tupleExp :: [Exp] -> Exp
tupleExp [] = con "()" []
tupleExp [e] = e
tupleExp (e : es) = ExpName Nothing (Name "pair") [e, tupleExp es]

pvar :: String -> Pat
pvar s = Pat (Name s) []

param :: String -> Ty -> Param
param n t = Typed False (Name n) t

fun :: String -> [Param] -> Ty -> Body -> FunDef
fun n ps ret body = FunDef False (sig' n ps ret) body

sig' :: String -> [Param] -> Ty -> Signature
sig' n ps ret = Signature [] [] (Name n) ps False (Just ret) False

inst :: String -> [Ty] -> Ty -> [FunDef] -> Instance
inst n paramTys mty = Instance False [] [] (Name n) paramTys mty

methodName :: Signature -> String
methodName (Signature _ _ n _ _ _ _) = pretty n

pName :: Param -> Name
pName (Typed _ n _) = n
pName (Untyped _ n) = n

pTy :: Param -> Ty
pTy (Typed _ _ t) = t
pTy (Untyped _ _) = error "interface method parameters must be typed"

retTy :: Signature -> Ty
retTy (Signature _ _ _ _ _ mret _) = maybe unitTy id mret

isVoid :: Signature -> Bool
isVoid (Signature _ _ _ _ _ mret _) = case mret of
  Nothing -> True
  Just (TyCon (Name "()") []) -> True
  _ -> False

canonicalSig :: Signature -> String
canonicalSig sig =
  methodName sig ++ "(" ++ intercalate "," (map (abiName . pTy) (sigParams sig)) ++ ")"

abiName :: Ty -> String
abiName t = case t of
  TyCon n []
    | pretty n == "word" -> "uint256"
    | pretty n `elem` valueTypes -> pretty n
  TyCon n [TyCon inner []]
    | pretty n == "memory", pretty inner == "string" -> "string"
    | pretty n == "memory", pretty inner == "bytes" -> "bytes"
  _ -> error ("interface desugar: unsupported ABI parameter type: " ++ pretty t)
  where
    valueTypes = ["uint256", "address", "bool", "bytes32", "bytes4"]

checkAllImplements :: [TopDecl] -> Either String ()
checkAllImplements ds = mapM_ checkContract [c | TContr c <- ds, contractKind c == ContractKind]
  where
    ifaceSigs :: [(String, [Signature])]
    ifaceSigs =
      [ (pretty (name c), [sig | CSignatureDecl _ sig <- decls c])
      | TContr c <- ds,
        contractKind c == InterfaceKind
      ]

    checkContract c = mapM_ (checkImpl c) (contractImplements c)

    checkImpl c ifaceName =
      case lookup (pretty ifaceName) ifaceSigs of
        Nothing ->
          Left $
            "contract " ++ pretty (name c) ++ " implements unknown interface " ++ pretty ifaceName
        Just methods -> mapM_ (checkMethod c ifaceName) methods

    checkMethod c ifaceName ifaceSig
      | any (abiMatches ifaceSig) (publicFns c) = Right ()
      | otherwise =
          Left $
            "contract "
              ++ pretty (name c)
              ++ " does not implement `"
              ++ methodName ifaceSig
              ++ "("
              ++ intercalate "," (map (abiName . pTy) (sigParams ifaceSig))
              ++ ") returns ("
              ++ pretty (retTy ifaceSig)
              ++ ")` required by interface "
              ++ pretty ifaceName

    publicFns c = [funSignature fd | CFunDecl fd <- decls c, funIsPublic fd]

abiMatches :: Signature -> Signature -> Bool
abiMatches a b =
  methodName a == methodName b
    && map (abiName . pTy) (sigParams a) == map (abiName . pTy) (sigParams b)
    && pretty (retTy a) == pretty (retTy b)
