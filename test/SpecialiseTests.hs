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

-- | Check whether a given type variable name has any binding in the subst.
hasAnyBind :: String -> TVSubst -> Bool
hasAnyBind name s = any (\(v, _) -> v == TVar (Name name)) (unTVSubst s)

-- ---------------------------------------------------------------------------
-- Test suite

specialiseTests :: TestTree
specialiseTests =
  testGroup
    "Specialise.resolveMPTCsFromPreds"
    [ testBind,
      testTwoBothPhantomExtras,
      testNopAFreeMain,
      testNopBIdempotent,
      testWrongInstance,
      testPartialInstance,
      testMixedConcreteAndPhantomExtra,
      testEmptyPredList,
      testMultiplePreds,
      testFirstWrongSecondBind,
      testPhantomChainSnapshot,
      testPhantomChainTwoCalls,
      testUnknownClass,
      testParameterisedMainTy
    ]

-- ---------------------------------------------------------------------------
-- BIND path: the core phantom-variable use case
--
-- Setting: function  forall a rep. a:Encoder(rep) => f(x:a) -> ()
-- 'a'   is visible  — it appears in the argument type, so the call site
--                     determines it: s0 = {a = Foo}.
-- 'rep' is phantom  — it does NOT appear in the function's monotype (a -> ()),
--                     so the call site gives no information about it.
-- resolveMPTCFromPreds must discover rep = word from the instance Foo:Encoder(word).

testBind :: TestTree
testBind = testCase "BIND: phantom extra bound from matching instance" $ do
  let inst = [] :=> inCls "Encoder" foo [word]
      env = mkTestEnv [(Name "Encoder", [inst])]
      s0 = TVar (Name "a") |-> foo -- only the visible param is in the subst
      pred_ = inCls "Encoder" (tv "a") [tv "rep"]
  s1 <- runResolveMPTCTest env s0 [pred_]
  assertBool "rep should be bound to word" (hasBind (TVar (Name "rep")) word s1)

-- ---------------------------------------------------------------------------
-- Two phantom extras — both free TyVars, both bound in one pass.
--
-- Setting: forall a out1 out2. a:BiEnc(out1, out2) => f(x:a) -> ()
-- Neither out1 nor out2 appears in the function type; both are phantom.
-- The single instance Foo:BiEnc(word, bool) pins both at once.

testTwoBothPhantomExtras :: TestTree
testTwoBothPhantomExtras =
  testCase "TWO-PHANTOM: two phantom extras both bound in one pass" $ do
    let inst = [] :=> inCls "BiEnc" foo [word, bool]
        env = mkTestEnv [(Name "BiEnc", [inst])]
        s0 = TVar (Name "a") |-> foo
        pred_ = inCls "BiEnc" (tv "a") [tv "out1", tv "out2"]
    s1 <- runResolveMPTCTest env s0 [pred_]
    assertBool "out1 should be bound to word" (hasBind (TVar (Name "out1")) word s1)
    assertBool "out2 should be bound to bool" (hasBind (TVar (Name "out2")) bool s1)

-- ---------------------------------------------------------------------------
-- NOP-A: main type still free → guard fails, nothing happens
--
-- Setting: the specialiser hasn't yet determined which concrete type 'a' is,
-- so the pred's main type is still a free TyVar after applying the current subst.
-- Without knowing the concrete self type, we can't look up the right instance.

testNopAFreeMain :: TestTree
testNopAFreeMain = testCase "NOP-A: free mainTy → does nothing" $ do
  let inst = [] :=> inCls "Encoder" foo [word]
      env = mkTestEnv [(Name "Encoder", [inst])]
      s0 = emptyTVSubst -- 'a' is not yet determined
      pred_ = inCls "Encoder" (tv "a") [tv "rep"]
  s1 <- runResolveMPTCTest env s0 [pred_]
  assertEqual "substitution unchanged when mainTy is free" s0 s1

-- ---------------------------------------------------------------------------
-- NOP-B: idempotency — phantom already bound in an earlier pass
--
-- Setting: resolveMPTCFromPreds was already called once and bound rep=word.
-- A second call with the same preds and the updated subst must be a no-op:
-- the guard  any (not . null . freetv) extras'  is False because
-- applytv {a=Foo, rep=word} [TyVar rep] = [word] (all concrete).

testNopBIdempotent :: TestTree
testNopBIdempotent = testCase "NOP-B: phantom already bound → idempotent, no change" $ do
  let inst = [] :=> inCls "Encoder" foo [word]
      env = mkTestEnv [(Name "Encoder", [inst])]
      s0 = TVar (Name "a") |-> foo <> TVar (Name "rep") |-> word
      pred_ = inCls "Encoder" (tv "a") [tv "rep"]
  s1 <- runResolveMPTCTest env s0 [pred_]
  assertEqual "substitution unchanged when phantom is already resolved" s0 s1

-- ---------------------------------------------------------------------------
-- WRONG-INSTANCE: no instance for the concrete self type → phantom stays free

testWrongInstance :: TestTree
testWrongInstance = testCase "WRONG-INSTANCE: no matching instance → phantom stays free" $ do
  let inst = [] :=> inCls "Encoder" bar [bool] -- instance for Bar, not Foo
      env = mkTestEnv [(Name "Encoder", [inst])]
      s0 = TVar (Name "a") |-> foo
      pred_ = inCls "Encoder" (tv "a") [tv "rep"]
  s1 <- runResolveMPTCTest env s0 [pred_]
  assertBool
    "rep should remain unbound when no instance matches"
    (not (hasAnyBind "rep" s1))

-- ---------------------------------------------------------------------------
-- PARTIAL: the matching instance is parametric in its extra → skipped
--
-- Instance  Foo : C(x, word)  has a free type variable x.
-- After specmatch succeeds for Foo, concreteExtras = [TyVar x, word] — x is still
-- free.  The guard  all (null . freetv) concreteExtras  fails → skipped.
-- We cannot deduce a unique concrete type for r1.

testPartialInstance :: TestTree
testPartialInstance = testCase "PARTIAL: parametric instance skipped, phantom stays free" $ do
  let instExtra1 = TyVar (TVar (Name "x"))
      instExtra2 = word
      inst = [] :=> inCls "C" foo [instExtra1, instExtra2]
      env = mkTestEnv [(Name "C", [inst])]
      s0 = TVar (Name "a") |-> foo
      pred_ = inCls "C" (tv "a") [tv "r1", tv "r2"]
  s1 <- runResolveMPTCTest env s0 [pred_]
  assertBool
    "r1 should remain unbound (parametric instance skipped)"
    (not (hasAnyBind "r1" s1))
  assertBool
    "r2 should remain unbound (whole instance skipped)"
    (not (hasAnyBind "r2" s1))

-- ---------------------------------------------------------------------------
-- Mixed: one extra fixed in the signature, one phantom
--
-- Setting: forall a rep. a:BiEnc(word, rep) => f(x:a) -> ()
-- The first extra is the concrete type 'word' written directly in the function's
-- constraint — it is not a phantom variable.  Only 'rep' is phantom.
-- The guard fires because extras' = [word, TyVar rep] has a free element.
-- The instance Foo:BiEnc(word, bool) provides rep = bool.

testMixedConcreteAndPhantomExtra :: TestTree
testMixedConcreteAndPhantomExtra =
  testCase "MIXED-EXTRA: fixed extra + phantom extra → only phantom bound" $ do
    let inst = [] :=> inCls "BiEnc" foo [word, bool]
        env = mkTestEnv [(Name "BiEnc", [inst])]
        s0 = TVar (Name "a") |-> foo
        pred_ = inCls "BiEnc" (tv "a") [word, tv "rep"]
    s1 <- runResolveMPTCTest env s0 [pred_]
    assertBool "rep should be bound to bool" (hasBind (TVar (Name "rep")) bool s1)
    assertBool
      "no spurious new binding introduced for word"
      (not (hasBind (TVar (Name "word")) bool s1))

-- ---------------------------------------------------------------------------
-- Empty pred list: trivial no-op

testEmptyPredList :: TestTree
testEmptyPredList = testCase "EMPTY: empty pred list → substitution unchanged" $ do
  let env = mkTestEnv []
      s0 = TVar (Name "a") |-> foo
  s1 <- runResolveMPTCTest env s0 []
  assertEqual "empty pred list leaves substitution unchanged" s0 s1

-- ---------------------------------------------------------------------------
-- Multiple preds: each phantom is bound from its own class
--
-- Setting: forall a b rep out. a:Encoder(rep), b:Decoder(out) => f(x:a, y:b) -> ()
-- a and b are visible; rep and out are both phantom.
-- After the call site pins a=Foo and b=Bar, one pass resolves both phantoms.

testMultiplePreds :: TestTree
testMultiplePreds = testCase "MULTI-PRED: independent phantoms across two preds both bound" $ do
  let instE = [] :=> inCls "Encoder" foo [word]
      instD = [] :=> inCls "Decoder" bar [bool]
      env = mkTestEnv [(Name "Encoder", [instE]), (Name "Decoder", [instD])]
      s0 = TVar (Name "a") |-> foo <> TVar (Name "b") |-> bar
      preds =
        [ inCls "Encoder" (tv "a") [tv "rep"],
          inCls "Decoder" (tv "b") [tv "out"]
        ]
  s1 <- runResolveMPTCTest env s0 preds
  assertBool "rep should be bound to word" (hasBind (TVar (Name "rep")) word s1)
  assertBool "out should be bound to bool" (hasBind (TVar (Name "out")) bool s1)

-- ---------------------------------------------------------------------------
-- Instance list iteration: first instance wrong, second correct
--
-- The class has two registered instances.  The first does not match the concrete
-- self type so specmatch returns Left and it is skipped.  The second matches and
-- the phantom extra is bound.

testFirstWrongSecondBind :: TestTree
testFirstWrongSecondBind =
  testCase "ITERATE: first instance wrong, second matches → phantom bound" $ do
    let instWrong = [] :=> inCls "Tagged" bar [bool] -- Bar ≠ Foo
        instRight = [] :=> inCls "Tagged" foo [word] -- Foo = Foo ✓
        env = mkTestEnv [(Name "Tagged", [instWrong, instRight])]
        s0 = TVar (Name "a") |-> foo
        pred_ = inCls "Tagged" (tv "a") [tv "rep"]
    s1 <- runResolveMPTCTest env s0 [pred_]
    assertBool
      "rep should be bound to word by the second instance"
      (hasBind (TVar (Name "rep")) word s1)

-- ---------------------------------------------------------------------------
-- Phantom chain — snapshot behaviour
--
-- Setting: forall a rep out. a:Encoder(rep), rep:Sink(out) => f(x:a) -> ()
-- 'a' is visible; 'rep' and 'out' are BOTH phantom.
-- 'rep' is the phantom extra of the first pred AND the phantom main type of
-- the second pred — they form a dependency chain.
--
-- resolveMPTCFromPreds captures a SNAPSHOT of the SM substitution at entry and
-- uses it for ALL preds in the same call.  Processing order:
--
--   pred1  Encoder (TyVar a) [TyVar rep]
--          snapshot applies: mainTy' = Foo (concrete) → BIND: rep = word
--          SM subst updated to {a=Foo, rep=word}
--
--   pred2  Sink (TyVar rep) [TyVar out]
--          snapshot still = {a=Foo} (captured before the loop)
--          mainTy' = applytv {a=Foo} (TyVar rep) = TyVar rep  (still free!)
--          NOP-A fires → out stays unbound
--
-- Consequence: a single call resolves rep but NOT out.

testPhantomChainSnapshot :: TestTree
testPhantomChainSnapshot =
  testCase "CHAIN-SNAPSHOT: chained phantom not resolved in one pass (snapshot)" $ do
    let instEnc = [] :=> inCls "Encoder" foo [word]
        instSink = [] :=> inCls "Sink" word [bool]
        env = mkTestEnv [(Name "Encoder", [instEnc]), (Name "Sink", [instSink])]
        s0 = TVar (Name "a") |-> foo
        preds =
          [ inCls "Encoder" (tv "a") [tv "rep"],
            inCls "Sink" (tv "rep") [tv "out"]
          ]
    s1 <- runResolveMPTCTest env s0 preds
    assertBool "rep resolved from pred1" (hasBind (TVar (Name "rep")) word s1)
    assertBool
      "out NOT resolved in one pass (snapshot does not see rep=word)"
      (not (hasAnyBind "out" s1))

-- ---------------------------------------------------------------------------
-- Phantom chain — two calls resolve the full chain
--
-- A second call with the updated substitution now sees rep=word in the snapshot,
-- so pred2's mainTy' becomes 'word' (concrete) and out = bool is bound.

testPhantomChainTwoCalls :: TestTree
testPhantomChainTwoCalls =
  testCase "CHAIN-TWO-CALLS: second pass resolves the chained phantom" $ do
    let instEnc = [] :=> inCls "Encoder" foo [word]
        instSink = [] :=> inCls "Sink" word [bool]
        env = mkTestEnv [(Name "Encoder", [instEnc]), (Name "Sink", [instSink])]
        s0 = TVar (Name "a") |-> foo
        preds =
          [ inCls "Encoder" (tv "a") [tv "rep"],
            inCls "Sink" (tv "rep") [tv "out"]
          ]
    s1 <- runResolveMPTCTest env s0 preds -- first pass: binds rep=word
    s2 <- runResolveMPTCTest env s1 preds -- second pass: now rep is in snapshot → binds out=bool
    assertBool
      "out should be bound to bool after the second pass"
      (hasBind (TVar (Name "out")) bool s2)

-- ---------------------------------------------------------------------------
-- Unknown class: class absent from instEnv → no-op

testUnknownClass :: TestTree
testUnknownClass = testCase "UNKNOWN-CLASS: class not in instEnv → phantom stays free" $ do
  let env = mkTestEnv []
      s0 = TVar (Name "a") |-> foo
      pred_ = inCls "UnknownClass" (tv "a") [tv "rep"]
  s1 <- runResolveMPTCTest env s0 [pred_]
  assertBool
    "rep should remain unbound when class has no instances"
    (not (hasAnyBind "rep" s1))

-- ---------------------------------------------------------------------------
-- Parameterised self type
--
-- The self type is List(word) — a parameterised TyCon, not a nullary one.
-- The instance is  forall a. List(a):Container(bool), stored as
-- instMainTy = List(TyVar a), instExtras = [bool].
-- specmatch (List(TyVar a)) (List(word)) = Right {a=word}, concreteExtras = [bool],
-- then specmatch (TyVar rep) bool = Right {rep=bool}.

testParameterisedMainTy :: TestTree
testParameterisedMainTy =
  testCase "PARAM-MAIN: parameterised self type → phantom bound" $ do
    let listWord = TyCon (Name "List") [word]
        listA = TyCon (Name "List") [tv "a"]
        inst = [] :=> inCls "Container" listA [bool]
        env = mkTestEnv [(Name "Container", [inst])]
        s0 = TVar (Name "self") |-> listWord
        pred_ = inCls "Container" (tv "self") [tv "rep"]
    s1 <- runResolveMPTCTest env s0 [pred_]
    assertBool
      "rep should be bound to bool via List(a):Container(bool) instance"
      (hasBind (TVar (Name "rep")) bool s1)
