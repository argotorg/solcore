module SpecialiseTests (specialiseTests) where

import Data.Map.Strict qualified as Map
import Solcore.Backend.Specialise (TVSubst (..), emptyTVSubst, runResolveMPTCTest, (|->))
import Solcore.Frontend.Syntax.Name (Name (..))
import Solcore.Frontend.Syntax.Ty (Pred (..), Qual (..), Ty (..), Tyvar (..))
import Solcore.Frontend.TypeInference.TcEnv (TcEnv (..), initTcEnv)
import Solcore.Pipeline.Options (stdOpt)
import Test.Tasty
import Test.Tasty.HUnit

-- ---------------------------------------------------------------------------
-- Helpers

word :: Ty
word = TyCon (Name "word") []

foo :: Ty
foo = TyCon (Name "Foo") []

bar :: Ty
bar = TyCon (Name "Bar") []

bool :: Ty
bool = TyCon (Name "bool") []

tv :: String -> Ty
tv s = TyVar (TVar (Name s))

inCls :: String -> Ty -> [Ty] -> Pred
inCls cls main extras = InCls (Name cls) main extras

-- | Build a test TcEnv whose instance table is exactly the given mapping.
-- The rest of the environment uses stdOpt defaults.
mkTestEnv :: [(Name, [Qual Pred])] -> TcEnv
mkTestEnv insts = (initTcEnv stdOpt) {instEnv = Map.fromList insts}

-- | Check whether (v, t) appears in a substitution.
hasBind :: Tyvar -> Ty -> TVSubst -> Bool
hasBind v t s = (v, t) `elem` unTVSubst s

-- ---------------------------------------------------------------------------
-- Test suite

specialiseTests :: TestTree
specialiseTests =
  testGroup
    "Specialise.resolveMPTCsFromPreds"
    [ testBind
    , testNopAFreeMain
    , testNopBExtrasAlreadyConcrete
    , testWrongInstance
    , testPartialInstance
    , testMultiExtraOnlyFreeOneGetsResolved
    ]

-- ---------------------------------------------------------------------------
-- BIND path: the happy path

-- | When mainTy is concrete and at least one extra is a free TyVar,
-- tryResolveMPTC should look up the matching instance and bind the extra.
testBind :: TestTree
testBind = testCase "BIND: concrete mainTy + free extra → binds extra to instance value" $ do
  let inst  = [] :=> inCls "Encoder" foo [word]
      env   = mkTestEnv [(Name "Encoder", [inst])]
      s0    = TVar (Name "a") |-> foo
      pred_ = inCls "Encoder" foo [tv "rep"]
  s1 <- runResolveMPTCTest env s0 [pred_]
  assertBool "rep should be bound to word after resolution"
    (hasBind (TVar (Name "rep")) word s1)

-- ---------------------------------------------------------------------------
-- NOP-A path: mainTy still free → guard fails, nothing happens

-- | When mainTy has free type variables the guard
--   null (freetv mainTy') is False and tryResolveMPTC is NOT called.
testNopAFreeMain :: TestTree
testNopAFreeMain = testCase "NOP-A: free mainTy → does nothing" $ do
  let inst  = [] :=> inCls "Encoder" foo [word]
      env   = mkTestEnv [(Name "Encoder", [inst])]
      s0    = emptyTVSubst
      pred_ = inCls "Encoder" (tv "a") [tv "rep"]  -- 'a' is still free
  s1 <- runResolveMPTCTest env s0 [pred_]
  assertEqual "substitution should be unchanged when mainTy is free" s0 s1

-- ---------------------------------------------------------------------------
-- NOP-B path: extras already concrete after applying the SM substitution

-- | When the SM substitution already maps every extra to a concrete type,
--   any (not . null . freetv) extras' is False and tryResolveMPTC is NOT called.
testNopBExtrasAlreadyConcrete :: TestTree
testNopBExtrasAlreadyConcrete = testCase "NOP-B: extras already concrete in SM subst → does nothing" $ do
  let inst  = [] :=> inCls "Encoder" foo [word]
      env   = mkTestEnv [(Name "Encoder", [inst])]
      s0    = TVar (Name "a") |-> foo <> TVar (Name "rep") |-> word
      pred_ = inCls "Encoder" foo [tv "rep"]  -- 'rep' is free in pred but bound in s0
  s1 <- runResolveMPTCTest env s0 [pred_]
  -- s0 applied to [TyVar rep] gives [word] (concrete) → guard is false → s1 == s0
  assertEqual "substitution should be unchanged when all extras are already concrete" s0 s1

-- ---------------------------------------------------------------------------
-- WRONG-INSTANCE path: no matching instance → nothing changes

-- | When the instance table only contains an instance for Bar (not Foo),
--   specmatch fails for every instance and no binding is added.
testWrongInstance :: TestTree
testWrongInstance = testCase "WRONG-INSTANCE: no matching instance → does nothing" $ do
  let inst  = [] :=> inCls "Encoder" bar [bool]  -- instance for Bar, not Foo
      env   = mkTestEnv [(Name "Encoder", [inst])]
      s0    = emptyTVSubst
      pred_ = inCls "Encoder" foo [tv "rep"]
  s1 <- runResolveMPTCTest env s0 [pred_]
  assertEqual "substitution should be unchanged when no instance matches" s0 s1

-- ---------------------------------------------------------------------------
-- PARTIAL path: matching instance has free vars in its extra → skipped

-- | Instance head  Foo : C(x, word)  has a free type variable x.
--   After matching Foo against the concrete mainTy we get an empty substitution,
--   so concreteExtras = [TyVar x, word].  The guard
--     all (null . freetv) concreteExtras
--   is False → the instance is skipped and nothing is bound.
testPartialInstance :: TestTree
testPartialInstance = testCase "PARTIAL: instance extras have free vars → skipped" $ do
  let instExtra1 = TyVar (TVar (Name "x"))  -- free variable inside instance
      instExtra2 = word
      inst        = [] :=> inCls "C" foo [instExtra1, instExtra2]
      env         = mkTestEnv [(Name "C", [inst])]
      s0          = emptyTVSubst
      pred_       = inCls "C" foo [tv "r1", tv "r2"]
  s1 <- runResolveMPTCTest env s0 [pred_]
  assertEqual "substitution should be unchanged when instance extras have free vars" s0 s1

-- ---------------------------------------------------------------------------
-- Multi-extra: only the free extra gets resolved, the concrete one is untouched

-- | Pred has two extras: one already concrete (word), one free (rep).
--   The guard fires because at least one extra is free.
--   tryResolveMPTC should bind rep = bool from the instance.
testMultiExtraOnlyFreeOneGetsResolved :: TestTree
testMultiExtraOnlyFreeOneGetsResolved =
  testCase "MULTI-EXTRA: only the free extra is bound, concrete one is left alone" $ do
    let inst  = [] :=> inCls "C2" foo [word, bool]
        env   = mkTestEnv [(Name "C2", [inst])]
        s0    = emptyTVSubst
        pred_ = inCls "C2" foo [word, tv "rep"]  -- word is concrete, rep is free
    s1 <- runResolveMPTCTest env s0 [pred_]
    assertBool "rep should be bound to bool after resolution"
      (hasBind (TVar (Name "rep")) bool s1)
    -- word is already concrete in the pred, specmatch (word, word) = Right {} → no change
    assertBool "no spurious bindings for the already-concrete extra"
      (not (hasBind (TVar (Name "word")) bool s1))
