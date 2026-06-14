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
-- The helper hands back a type-level token — @Proxy(methods)@ — describing the
-- contract's public methods as a right-nested tuple terminated by @()@:
--
--   @Proxy((Method(...), (Method(...), ... ())))@
--
-- Each element carries the very same @Method(name,payability,args,rets,fn)@
-- typing consumed by @Selector.compute@ (see @std/dispatch.solc@), so no
-- selector hashing leaks into the compiler.  Walking that tuple — counting the
-- methods (@length@) and XOR-folding their selectors into an interface id — is
-- the @PublicMethods@ type class in @std/dispatch.solc@; the compiler only
-- exposes the method list, never the iteration or hashing.
--
-- This must run BEFORE contract dispatch generation, which produces the
-- per-method @DispatchNameTy_*@ name types (and their @SigString@ instances)
-- that the method tuple refers to.
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
import Solcore.Primitives.Primitives (tupleTyFromList, unit)

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

-- | Generate the helper that yields a contract's public-method tuple as a
-- @Proxy@ type token.  The tuple is right-nested and terminated by @()@ so the
-- @PublicMethods@ instances in @std/dispatch.solc@ only need a @()@ base case
-- and an @(n, m)@ recursive case (no special single-method case).
genPublicMethodsFn :: Contract Name -> TopDecl Name
genPublicMethodsFn c@(Contract cname _ _) =
  TFunDef (FunDef False sig body)
  where
    -- the public methods, plus a `()` terminator for the tuple
    methodsTuple = tupleTyFromList (publicMethodTypes c ++ [unit])
    proxyTy = TyCon "Proxy" [methodsTuple]

    sig =
      Signature
        { sigVars = [],
          sigContext = [],
          sigName = publicMethodsTagName cname,
          sigParams = [],
          sigRetComptime = False,
          sigReturn = Just proxyTy,
          sigPayable = False
        }

    -- return Proxy : Proxy((Method(...), (Method(...), ... ())));
    body = [Return (TyExp (Con "Proxy" []) proxyTy)]
