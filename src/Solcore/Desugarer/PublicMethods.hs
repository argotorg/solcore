{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Solcore.Desugarer.PublicMethods
-- Description : Implements the `type(C).publicMethods` primitive
--
-- The parser/name-resolver turns `type(C).publicMethods` into a call to a
-- per-contract helper function (see 'publicMethodsTagName').  This pass
-- generates the body of that helper for every contract whose primitive is
-- actually used.
--
-- The helper builds a runtime @memory(DynArray(bytes4))@ holding the selector
-- of each public method, computed via the dispatcher's existing
-- @Selector.compute@ instance (which reuses @sigStr@/@SigString@).  Folding
-- those selectors with XOR — the interface-id computation itself — lives in
-- @std/dispatch.solc@ as 'calculateInterfaceId', so all hashing and selector
-- logic stays in the standard library, never in the compiler.
--
-- This must run AFTER contract dispatch generation, which produces the
-- per-method @DispatchNameTy_*@ name types (and their @SigString@ instances)
-- that the generated selectors refer to.
module Solcore.Desugarer.PublicMethods
  ( publicMethodsDesugarer,
    publicMethodsTopDecls,
  )
where

import Data.Generics (listify)
import Data.List (isPrefixOf)
import Solcore.Desugarer.ContractDispatch (publicMethodTypes)
import Solcore.Frontend.Syntax
import Solcore.Frontend.Syntax.NameResolution (publicMethodsTagName)

publicMethodsDesugarer :: CompUnit Name -> CompUnit Name
publicMethodsDesugarer (CompUnit ims topdecls) =
  CompUnit ims (publicMethodsTopDecls topdecls)

publicMethodsTopDecls :: [TopDecl Name] -> [TopDecl Name]
publicMethodsTopDecls topdecls = topdecls ++ helpers
  where
    -- every contract paired with the helper name its `publicMethods` primitive
    -- would call
    contractsByTag =
      [(publicMethodsTagName cname, c) | TContr c@(Contract cname _ _) <- topdecls]

    -- helper names actually referenced by a `type(C).publicMethods` call
    referenced =
      [fn | Call Nothing fn [] <- listify isTagCall topdecls]

    helpers =
      [ genPublicMethodsFn c
      | (tag, c) <- contractsByTag,
        tag `elem` referenced
      ]

isTagCall :: Exp Name -> Bool
isTagCall (Call Nothing fn []) = isTagName fn
isTagCall _ = False

isTagName :: Name -> Bool
isTagName (Name s) = "$publicMethods$" `isPrefixOf` s
isTagName _ = False

-- | Generate the helper function that builds the public-method selector array
-- for a contract.
genPublicMethodsFn :: Contract Name -> TopDecl Name
genPublicMethodsFn c@(Contract cname _ _) =
  TFunDef (FunDef sig body)
  where
    methodTys = publicMethodTypes c
    n = length methodTys

    bytes4Ty = TyCon "bytes4" []
    uint256Ty = TyCon "uint256" []
    arrTy = TyCon "memory" [TyCon "DynArray" [bytes4Ty]]

    sig =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = publicMethodsTagName cname,
          sigParams = [],
          sigRetComptime = False,
          sigReturn = Just arrTy,
          sigPayable = False
        }

    -- let arr : memory(DynArray(bytes4)) = allocateDynamicArray(Proxy, n);
    letArr =
      Let
        False
        "arr"
        (Just arrTy)
        ( Just
            ( Call
                Nothing
                "allocateDynamicArray"
                [proxyExp bytes4Ty, Lit (IntLit (toInteger n))]
            )
        )

    -- IndexAccess.set(arr, (Typedef.abs(i) : uint256), Selector.compute(Proxy:Proxy(Method(...))));
    setStmt i mty =
      StmtExp
        ( Call
            Nothing
            (QualName "IndexAccess" "set")
            [ Var "arr",
              TyExp (Call Nothing (QualName "Typedef" "abs") [Lit (IntLit (toInteger i))]) uint256Ty,
              Call Nothing (QualName "Selector" "compute") [proxyExp mty]
            ]
        )

    body =
      [letArr]
        ++ zipWith setStmt [0 :: Integer ..] methodTys
        ++ [Return (Var "arr")]

proxyExp :: Ty -> Exp Name
proxyExp t = TyExp (Con "Proxy" []) (TyCon "Proxy" [t])
