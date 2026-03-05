module MatchCompilerTests where

import Data.List (sort)
import Data.Map qualified as Map
import Solcore.Desugarer.DecisionTreeCompiler
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Test.Tasty
import Test.Tasty.HUnit

matchTests :: TestTree
matchTests =
  testGroup
    "DecisionTreeCompiler"
    [ testGroup
        "compileMatrix"
        [ test_emptyMatrix_isFail_andNonExhaustive,
          test_singleVarRow_isLeaf_noWarnings,
          test_redundantVarRow_emitsRedundantClause,
          test_boolCompleteCover_noDefault_noWarnings,
          test_boolIncompleteCover_defaultFail_nonExhaustive,
          test_listCompleteCover_nestedSwitch_noWarnings,
          test_twoColumn_completeCover_noWarnings,
          test_litPats_withVarDefault_noWarnings,
          test_litPats_noDefault_nonExhaustive,
          test_unknownConstructor_isError
        ]
    ]

runMatrix ::
  TypeEnv ->
  [Ty] ->
  [Occurrence] ->
  PatternMatrix ->
  [Action] ->
  IO (Either String (DecisionTree, [Warning]))
runMatrix env tys occs matrix acts =
  runCompilerM env (compileMatrix tys occs matrix acts)

assertRight ::
  String ->
  IO (Either String (DecisionTree, [Warning])) ->
  (DecisionTree -> [Warning] -> Assertion) ->
  Assertion
assertRight label act check = do
  r <- act
  case r of
    Left err -> assertFailure (label ++ ": unexpected error: " ++ err)
    Right (tree, ws) -> check tree ws

assertLeft ::
  String ->
  IO (Either String (DecisionTree, [Warning])) ->
  Assertion
assertLeft label act = do
  r <- act
  case r of
    Left _ -> pure ()
    Right (tree, _) ->
      assertFailure (label ++ ": expected error but got tree: " ++ show tree)

isNonExh :: Warning -> Bool
isNonExh (NonExhaustive _ _) = True
isNonExh _ = False

isRedundant :: Warning -> Bool
isRedundant (RedundantClause _ _ _) = True
isRedundant _ = False

branchNames :: [(Id, [Pattern], DecisionTree)] -> [String]
branchNames bs = sort [nameOf (idName k) | (k, _, _) <- bs]
  where
    nameOf n = pretty n

branchLits :: [(Literal, DecisionTree)] -> [Literal]
branchLits = map fst

tyBool :: Ty
tyBool = TyCon (Name "Bool") []

tyList :: Ty
tyList = TyCon (Name "List") [tyBool]

tyWord :: Ty
tyWord = TyCon (Name "word") []

idTrue :: Id
idTrue = Id (Name "True") (funtype [] tyBool)

idFalse :: Id
idFalse = Id (Name "False") (funtype [] tyBool)

idNil :: Id
idNil = Id (Name "Nil") (funtype [] tyList)

idCons :: Id
idCons = Id (Name "Cons") (funtype [tyBool, tyList] tyList)

-- Type environments
boolEnv :: TypeEnv
boolEnv =
  Map.fromList
    [ (idName idTrue, ConInfo {conFieldTypes = [], conReturnType = tyBool}),
      (idName idFalse, ConInfo {conFieldTypes = [], conReturnType = tyBool})
    ]

listEnv :: TypeEnv
listEnv =
  Map.fromList
    [ (idName idNil, ConInfo {conFieldTypes = [], conReturnType = tyList}),
      (idName idCons, ConInfo {conFieldTypes = [tyBool, tyList], conReturnType = tyList})
    ]

patTrue :: Pattern
patTrue = PCon idTrue []

patFalse :: Pattern
patFalse = PCon idFalse []

patNil :: Pattern
patNil = PCon idNil []

patCons :: Pattern
patCons =
  PCon
    idCons
    [ PVar (Id (Name "h") tyBool),
      PVar (Id (Name "t") tyList)
    ]

pvar :: String -> Ty -> Pattern
pvar n ty = PVar (Id (Name n) ty)

varBool :: Exp Id
varBool = Var (Id (Name "x") tyBool)

varWord :: Exp Id
varWord = Var (Id (Name "n") tyWord)

retVar :: String -> Ty -> Action
retVar n ty = [Return (Var (Id (Name n) ty))]

actionA, actionB, actionC, actionD :: Action
actionA = retVar "a" tyBool
actionB = retVar "b" tyBool
actionC = retVar "c" tyBool
actionD = retVar "d" tyBool

occ1 :: [Occurrence]
occ1 = [[0]]

occ2 :: [Occurrence]
occ2 = [[0], [1]]

-- 1. Empty matrix: no rows at all → Fail + NonExhaustive
test_emptyMatrix_isFail_andNonExhaustive :: TestTree
test_emptyMatrix_isFail_andNonExhaustive =
  testCase "empty matrix -> Fail and one NonExhaustive warning" $
    assertRight "empty" (runMatrix boolEnv [tyBool] occ1 [] []) $
      \tree warns -> do
        assertEqual "tree should be Fail" Fail tree
        assertBool "should emit NonExhaustive warning" (any isNonExh warns)

-- 2. Single all-variable row → Leaf, no warnings
test_singleVarRow_isLeaf_noWarnings :: TestTree
test_singleVarRow_isLeaf_noWarnings =
  testCase "single variable row -> Leaf with no warnings"
    $ assertRight
      "single-var"
      (runMatrix boolEnv [tyBool] occ1 [[pvar "x" tyBool]] [actionA])
    $ \tree warns -> do
      case tree of
        Leaf _ act' -> assertEqual "leaf action should be actionA" actionA act'
        _ -> assertFailure ("expected Leaf, got: " ++ show tree)
      assertBool "should have no warnings" (null warns)

-- 3. Variable row that is redundant → Leaf + RedundantClause
test_redundantVarRow_emitsRedundantClause :: TestTree
test_redundantVarRow_emitsRedundantClause =
  testCase "variable row preceded by complete cover is marked redundant"
    $ assertRight
      "redundant"
      ( runMatrix
          boolEnv
          [tyBool]
          occ1
          [[pvar "x" tyBool], [patTrue], [patFalse]]
          [actionA, actionB, actionC]
      )
    $ \tree warns -> do
      case tree of
        Leaf _ _ -> pure ()
        _ -> assertFailure ("expected Leaf, got: " ++ show tree)
      assertBool "should emit RedundantClause warning" (any isRedundant warns)
      assertBool "should not emit NonExhaustive warning" (not (any isNonExh warns))

-- 4. Complete Bool cover → Switch with Nothing default, no warnings
test_boolCompleteCover_noDefault_noWarnings :: TestTree
test_boolCompleteCover_noDefault_noWarnings =
  testCase "True+False cover -> Switch with no default and no warnings"
    $ assertRight
      "bool-complete"
      ( runMatrix
          boolEnv
          [tyBool]
          occ1
          [[patTrue], [patFalse]]
          [actionA, actionB]
      )
    $ \tree warns -> do
      case tree of
        Switch [0] branches Nothing -> do
          assertEqual
            "should have exactly True and False branches"
            ["False", "True"]
            (branchNames branches)
          assertBool "should have no warnings" (null warns)
        Switch _ _ (Just _) ->
          assertFailure "complete cover must not have a default branch"
        _ ->
          assertFailure ("expected Switch, got: " ++ show tree)

-- 5. Incomplete Bool cover → Switch with Just Fail default + NonExhaustive
test_boolIncompleteCover_defaultFail_nonExhaustive :: TestTree
test_boolIncompleteCover_defaultFail_nonExhaustive =
  testCase "True-only cover -> Switch with Fail default and NonExhaustive warning"
    $ assertRight
      "bool-incomplete"
      (runMatrix boolEnv [tyBool] occ1 [[patTrue]] [actionA])
    $ \tree warns -> do
      case tree of
        Switch [0] branches (Just Fail) -> do
          assertEqual
            "should only have the True branch"
            ["True"]
            (branchNames branches)
          assertBool "should emit NonExhaustive warning" (any isNonExh warns)
        _ ->
          assertFailure ("expected Switch … (Just Fail), got: " ++ show tree)

-- 6. Complete List cover (Nil + Cons) → Switch, no default, no warnings
test_listCompleteCover_nestedSwitch_noWarnings :: TestTree
test_listCompleteCover_nestedSwitch_noWarnings =
  testCase "Nil + Cons cover -> Switch with no default and no warnings"
    $ assertRight
      "list-complete"
      ( runMatrix
          listEnv
          [tyList]
          occ1
          [[patNil], [patCons]]
          [actionA, actionB]
      )
    $ \tree warns -> do
      case tree of
        Switch [0] branches Nothing -> do
          assertEqual
            "should have exactly Cons and Nil branches"
            ["Cons", "Nil"]
            (branchNames branches)
          assertBool "should have no warnings" (null warns)
        _ ->
          assertFailure ("expected Switch with two branches, got: " ++ show tree)

-- 7. Two-column Bool×Bool complete matrix → no default at any level, no warnings
test_twoColumn_completeCover_noWarnings :: TestTree
test_twoColumn_completeCover_noWarnings =
  testCase "Bool*Bool all-four-cases matrix -> no defaults, no warnings"
    $ assertRight
      "bool*bool"
      ( runMatrix
          boolEnv
          [tyBool, tyBool]
          occ2
          [ [patTrue, patTrue],
            [patTrue, patFalse],
            [patFalse, patTrue],
            [patFalse, patFalse]
          ]
          [actionA, actionB, actionC, actionD]
      )
    $ \tree warns -> do
      case tree of
        Switch _ outerBranches Nothing -> do
          mapM_ (\(_, _, t) -> assertInnerHasNoDefault t) outerBranches
          assertBool "should have no warnings" (null warns)
        _ ->
          assertFailure
            ( "expected top-level Switch with no default, got: "
                ++ show tree
            )
  where
    assertInnerHasNoDefault (Switch _ _ Nothing) = pure ()
    assertInnerHasNoDefault t =
      assertFailure ("inner subtree should have no default branch, got: " ++ show t)

-- 8. Literal patterns with a variable catch-all → LitSwitch with Leaf default, no warnings
test_litPats_withVarDefault_noWarnings :: TestTree
test_litPats_withVarDefault_noWarnings =
  testCase "lit patterns with variable fallback -> LitSwitch with Leaf default, no warnings"
    $ assertRight
      "lit-default"
      ( runMatrix
          Map.empty
          [tyWord]
          occ1
          [[PLit (IntLit 0)], [PLit (IntLit 1)], [pvar "x" tyWord]]
          [actionA, actionB, actionC]
      )
    $ \tree warns -> do
      case tree of
        LitSwitch [0] branches (Just (Leaf _ defAct)) -> do
          assertBool
            "should have literal 0 branch"
            (IntLit 0 `elem` branchLits branches)
          assertBool
            "should have literal 1 branch"
            (IntLit 1 `elem` branchLits branches)
          assertEqual "default leaf should be actionC" actionC defAct
          assertBool "should have no warnings:" (null warns)
        LitSwitch _ _ Nothing ->
          assertFailure "expected a default branch for the variable catch-all"
        _ ->
          assertFailure ("expected LitSwitch, got: " ++ show tree)

-- 9. Literal patterns without a catch-all → LitSwitch with Fail default + NonExhaustive
test_litPats_noDefault_nonExhaustive :: TestTree
test_litPats_noDefault_nonExhaustive =
  testCase "lit patterns without catch-all -> LitSwitch with Fail default and NonExhaustive"
    $ assertRight
      "lit-no-default"
      ( runMatrix
          Map.empty
          [tyWord]
          occ1
          [[PLit (IntLit 0)], [PLit (IntLit 1)]]
          [actionA, actionB]
      )
    $ \tree warns -> do
      case tree of
        LitSwitch [0] branches (Just Fail) -> do
          assertBool
            "should have literal 0 branch"
            (IntLit 0 `elem` branchLits branches)
          assertBool
            "should have literal 1 branch"
            (IntLit 1 `elem` branchLits branches)
          assertBool "should emit NonExhaustive warning" (any isNonExh warns)
        _ ->
          assertFailure ("expected LitSwitch … (Just Fail), got: " ++ show tree)

-- 10. Constructor absent from the TypeEnv → Left error
test_unknownConstructor_isError :: TestTree
test_unknownConstructor_isError =
  testCase "pattern constructor absent from TypeEnv -> Left error" $
    assertLeft
      "unknown-con"
      (runMatrix Map.empty [tyBool] occ1 [[patTrue]] [actionA])
