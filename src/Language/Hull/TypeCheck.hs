{-# LANGUAGE OverloadedStrings #-}

module Language.Hull.TypeCheck
  ( checkObject,
    checkBody,
    checkStmt,
    checkExpr,
  )
where

import Control.Monad (forM_, unless, when, zipWithM_)
import Control.Monad.State (gets)
import Data.List (stripPrefix)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Language.Hull
import Language.Hull.TcEnv
import Language.Hull.TcMonad
import Language.Hull.Types
import Language.Yul
import Solcore.Frontend.Syntax.Name qualified as SName

-- Entry point

checkObject :: Object -> HullTcM ()
checkObject (Object _ code inners) = do
-- Pre-scan registers all SFunction signatures before type-checking bodies.
-- This allows forward references and mutual recursion between Hull functions.
  preScanBody code
  checkBody code
  mapM_ checkObject inners

-- Type-check a sequence of statements sequentially.
checkBody :: Body -> HullTcM ()
checkBody = mapM_ checkStmt

-- Register every SFunction signature in a body without checking their bodies.
-- Recurses into SBlock so nested scopes are also pre-scanned.
preScanBody :: Body -> HullTcM ()
preScanBody = mapM_ preScanStmt

preScanStmt :: Stmt -> HullTcM ()
preScanStmt (SFunction name args ret _) = do
  let sig = HullFunSig {hsig_args = map argType args, hsig_ret = ret}
  extendFun name sig
preScanStmt (SBlock stmts) = preScanBody stmts
preScanStmt _ = pure ()

-- Statements

checkStmt :: Stmt -> HullTcM ()
checkStmt (SAlloc x t) =
  extendVar x t
checkStmt (SAssign lhs rhs) = do
  lhsTy <- checkExpr lhs
  rhsTy <- checkExpr rhs
  expectType lhsTy rhsTy
checkStmt (SReturn e) = do
  te <- checkExpr e
  mret <- getRetType
  case mret of
    Nothing -> hullError "return statement outside of a function"
    Just tr -> expectType tr te
checkStmt (SFunction name args ret body) = do
  let sig = HullFunSig {hsig_args = map argType args, hsig_ret = ret}
  extendFun name sig
  withLocalEnv $ do
    forM_ args $ \(TArg n t) -> extendVar n t
    withRetType ret (checkBody body)
checkStmt (SMatch ty e alts) = do
  te <- checkExpr e
  expectType ty te
  mapM_ (checkAlt (stripTypeName ty)) alts
checkStmt (SBlock stmts) =
  withLocalEnv (checkBody stmts)
checkStmt (SExpr e) =
  checkExpr e >> pure ()
checkStmt (SAssembly stmts) =
  withLocalEnv (checkAsmBlock stmts)
checkStmt (SRevert _) = pure ()
checkStmt (SComment _) = pure ()

argType :: Arg -> Type
argType (TArg _ t) = t

-- Expressions

checkExpr :: Expr -> HullTcM Type
checkExpr (EWord _) = pure TWord
checkExpr (EBool _) = pure TBool
checkExpr EUnit = pure TUnit
checkExpr (EVar x) = lookupVar x
checkExpr (EPair e1 e2) = TPair <$> checkExpr e1 <*> checkExpr e2
checkExpr (EFst e) = do
  t <- checkExpr e
  case stripTypeName t of
    TPair t1 _ -> pure t1
    _ -> hullError ("fst: expected pair type, got " ++ show t)
checkExpr (ESnd e) = do
  t <- checkExpr e
  case stripTypeName t of
    TPair _ t2 -> pure t2
    _ -> hullError ("snd: expected pair type, got " ++ show t)
checkExpr (EInl ty e) =
  case stripTypeName ty of
    TSum l _ -> do
      te <- checkExpr e
      expectType l te
      pure ty
    _ -> hullError ("inl: expected sum type annotation, got " ++ show ty)
checkExpr (EInr ty e) =
  case stripTypeName ty of
    TSum _ r -> do
      te <- checkExpr e
      expectType r te
      pure ty
    _ -> hullError ("inr: expected sum type annotation, got " ++ show ty)
checkExpr (EInK k ty e) =
  case stripTypeName ty of
    TSumN ts
      | k >= 0 && k < length ts -> do
          te <- checkExpr e
          expectType (ts !! k) te
          pure ty
      | otherwise ->
          hullError
            ( "inK: index "
                ++ show k
                ++ " out of range for sum type "
                ++ show ty
            )
    _ -> hullError ("inK: expected sum type annotation, got " ++ show ty)
checkExpr (ECall f args) = do
  sig <- lookupFun f
  let nExpected = length (hsig_args sig)
      nActual = length args
  when (nActual /= nExpected) $
    hullError $
      unlines
        [ "Arity mismatch in call to '" ++ f ++ "'",
          "  expected " ++ show nExpected ++ " argument(s)",
          "  found    " ++ show nActual
        ]
  argTypes <- mapM checkExpr args
  zipWithM_ expectType (hsig_args sig) argTypes
  pure (hsig_ret sig)
checkExpr (ECond ty cond e1 e2) = do
  tc <- checkExpr cond
  expectType TBool tc
  t1 <- checkExpr e1
  expectType ty t1
  t2 <- checkExpr e2
  expectType ty t2
  pure ty

-- Match alternatives

-- Type-check one alternative of a match expression.
-- The scrutinee type (already stripped of TNamed) is passed in.
checkAlt :: Type -> Alt -> HullTcM ()
checkAlt scrutTy (Alt pat bindName body) = do
  payTy <- payloadType scrutTy pat
  withLocalEnv $ do
    extendVar bindName payTy
    checkBody body

-- Compute the type of the payload variable bound in an alternative.
payloadType :: Type -> Pat -> HullTcM Type
payloadType (TSum t1 _) (PCon CInl) = pure t1
payloadType (TSum _ t2) (PCon CInr) = pure t2
payloadType (TSumN ts) (PCon (CInK k))
  | k >= 0 && k < length ts = pure (ts !! k)
  | otherwise = hullError ("Variant index " ++ show k ++ " out of range")
payloadType t PWildcard = pure t
payloadType TWord (PIntLit _) = pure TWord
payloadType t (PVar _) = pure t
payloadType t p =
  hullError
    ( "Pattern "
        ++ show p
        ++ " cannot match type "
        ++ show t
    )

-- Assembly type checking

checkAsmBlock :: [YulStmt] -> HullTcM ()
checkAsmBlock = mapM_ checkAsmStmt

checkAsmStmt :: YulStmt -> HullTcM ()
checkAsmStmt (YLet ns Nothing) =
  mapM_ (\n -> extendVar (show n) TWord) ns
checkAsmStmt (YLet ns (Just e)) = do
  t <- checkAsmExp e
  let nExpected = length ns
      nActual = returnCount t
  when (nActual /= nExpected) $
    hullError $
      unlines
        [ "Return count mismatch in let binding",
          "  binding    " ++ show nExpected ++ " variable(s)",
          "  expression returns " ++ show nActual ++ " value(s)"
        ]
  mapM_ (\n -> extendVar (show n) TWord) ns
checkAsmStmt (YAssign ns e) = do
  mapM_ (lookupVar . show) ns
  t <- checkAsmExp e
  let nExpected = length ns
      nActual = returnCount t
  when (nActual /= nExpected) $
    hullError $
      unlines
        [ "Return count mismatch in assignment",
          "  assigning  " ++ show nExpected ++ " variable(s)",
          "  expression returns " ++ show nActual ++ " value(s)"
        ]
checkAsmStmt (YIf cond body) = do
  checkAsmArg cond
  checkAsmBlock body
checkAsmStmt (YSwitch e cases mdef) = do
  checkAsmArg e
  forM_ cases $ \(_, body) -> checkAsmBlock body
  mapM_ checkAsmBlock mdef
checkAsmStmt (YFor pre cond post body) =
  -- In Yul, the init block shares scope with the condition, post, and body.
  withLocalEnv $ do
    checkAsmBlock pre
    checkAsmArg cond
    checkAsmBlock post
    checkAsmBlock body
checkAsmStmt (YBlock body) =
  withLocalEnv (checkAsmBlock body)
checkAsmStmt (YFun n args mrets body) = do
  let ret = nReturns (length (fromMaybe [] mrets))
      sig = HullFunSig (map (const TWord) args) ret
  extendFun (show n) sig
  withLocalEnv $ do
    mapM_ (\a -> extendVar (show a) TWord) args
    mapM_ (\r -> extendVar (show r) TWord) (fromMaybe [] mrets)
    checkAsmBlock body
checkAsmStmt (YExp e) = do
  t <- checkAsmExp e
  unless (typeEq t TUnit) $
    hullError $
      unlines
        [ "Expression used as a statement must return unit",
          "  got: " ++ show t,
          "  hint: use pop() to discard a word value"
        ]
checkAsmStmt YBreak = pure ()
checkAsmStmt YContinue = pure ()
checkAsmStmt YLeave = pure ()
checkAsmStmt (YComment _) = pure ()

checkAsmExp :: YulExp -> HullTcM Type
checkAsmExp (YLit _) = pure TWord
checkAsmExp (YMeta _) = pure TWord
checkAsmExp (YIdent n) = lookupVar (show n)
checkAsmExp (YCall f args) = do
  sig <- lookupAsmFun f
  let nExpected = length (hsig_args sig)
      nActual = length args
  when (nActual /= nExpected) $
    hullError $
      unlines
        [ "Arity mismatch in call to " ++ show f,
          "  expected " ++ show nExpected ++ " argument(s)",
          "  found    " ++ show nActual
        ]
  mapM_ checkAsmArg args
  pure (hsig_ret sig)

checkAsmArg :: YulExp -> HullTcM ()
checkAsmArg e = do
  t <- checkAsmExp e
  if isWordType t
    then pure ()
    else
      if typeEq t TUnit
        then hullError "Void expression cannot be used as a function argument"
        else hullError ("Expected a single word argument, got: " ++ show t)

-- Look up a function called from assembly.
-- Hull user functions are called with the usr$ prefix, so strip it before
-- looking up in hull_funs where they are stored without the prefix.
-- The returned sig is normalized: all args become TWord, and non-unit return
-- types collapse to TWord (matching how Hull signatures are projected into Yul).
lookupAsmFun :: SName.Name -> HullTcM HullFunSig
lookupAsmFun f = do
  funs <- gets hull_funs
  let s = show f
      key = fromMaybe s (stripPrefix "usr$" s)
  case Map.lookup key funs of
    Just sig ->
      pure $
        HullFunSig
          (map (const TWord) (hsig_args sig))
          (if typeEq (hsig_ret sig) TUnit then TUnit else TWord)
    Nothing -> hullError ("Unknown function in assembly: " ++ s)
