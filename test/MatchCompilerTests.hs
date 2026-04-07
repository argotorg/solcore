module MatchCompilerTests where

import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Solcore.Desugarer.DecisionTreeCompiler
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Primitives.Primitives (at, bt, pair, sumTy)
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
        ],
      testGroup
        "treeToStmt"
        [ test_treeToStmt_singleVar_substituted,
          test_treeToStmt_multiColumn_varsBoundViaSpecialize
        ],
      testGroup
        "redundancy warnings"
        [ test_allVar_first_shadows_nonexhaustive_rest,
          test_twoCol_noFalsePositive_partialOverlap,
          test_twoCol_genuinelyRedundant_thirdRow,
          test_singleCol_duplicateRow_warned
        ],
      testGroup
        "non-exhaustive witness completeness"
        [ test_twoCol_con_nonExh_witness_has_both_columns,
          test_twoCol_lit_nonExh_witness_has_both_columns
        ],
      testGroup
        "sibling set / missing-constructor witness"
        [ test_nonExh_polyEnv_missingNil_witness_is_Nil,
          test_nonExh_polyEnv_missingCons_witness_is_Cons
        ],
      testGroup
        "inhabitsM correctness"
        [ test_inhabitsM_wildcard_row_covers_all
        ],
      testGroup
        "primEnv correctness"
        [ test_pairConInfo_returnType_is_result_type,
          test_inlConInfo_returnType_is_result_type,
          test_inrConInfo_returnType_is_result_type
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
  runCompilerM env (compileMatrix tys occs matrix [([], a) | a <- acts])

-- Like runMatrix but also runs the redundancy pre-pass so RedundantClause
-- warnings are emitted (mirroring what compile (Match …) does end-to-end).
runMatrixFull ::
  TypeEnv ->
  [Ty] ->
  [Occurrence] ->
  PatternMatrix ->
  [Action] ->
  IO (Either String (DecisionTree, [Warning]))
runMatrixFull env tys occs matrix acts =
  runCompilerM env $ do
    ctx <- askWarnCtx
    let bacts = [([], a) | a <- acts]
    checkRedundancy ctx tys matrix bacts
    compileMatrix tys occs matrix bacts

-- Run compileMatrix then treeToStmt in one pass, returning the compiled statement.
runFull ::
  TypeEnv ->
  OccMap ->
  [Ty] ->
  [Occurrence] ->
  PatternMatrix ->
  [Action] ->
  IO (Either String (Stmt Id, [Warning]))
runFull env occMap tys occs matrix acts =
  runCompilerM env $ do
    tree <- compileMatrix tys occs matrix [([], a) | a <- acts]
    treeToStmt occMap tree

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

branchLits :: [(AtomicPat, DecisionTree)] -> [Literal]
branchLits = mapMaybe (either Just (const Nothing) . fst)

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

-- Polymorphic list type, as addDataTyInfo produces for: data List a = Nil | Cons a (List a)
-- conReturnType stores TyCon "List" [TyVar "a"], NOT the concrete TyCon "List" [Bool].
tyVarA :: Ty
tyVarA = TyVar (TVar (Name "a"))

tyListPoly :: Ty
tyListPoly = TyCon (Name "List") [tyVarA]

-- TypeEnv as initialTypeEnv/addDataTyInfo would build it (polymorphic return types).
polyListEnv :: TypeEnv
polyListEnv =
  Map.fromList
    [ (Name "Nil", ConInfo {conFieldTypes = [], conReturnType = tyListPoly}),
      (Name "Cons", ConInfo {conFieldTypes = [tyVarA, tyListPoly], conReturnType = tyListPoly})
    ]

-- Constructor Ids as the type-checker annotates them in patterns (concrete types).
idNilConcrete :: Id
idNilConcrete = Id (Name "Nil") tyList -- tyList = TyCon "List" [tyBool]

idConsConcrete :: Id
idConsConcrete = Id (Name "Cons") (funtype [tyBool, tyList] tyList)

patNilConcrete :: Pattern
patNilConcrete = PCon idNilConcrete []

patConsConcrete :: Pattern
patConsConcrete =
  PCon
    idConsConcrete
    [ pvar "h" tyBool,
      pvar "t" tyList
    ]

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

-- 3. All-var first row always fires; later rows are dead code → warned as unreachable
test_redundantVarRow_emitsRedundantClause :: TestTree
test_redundantVarRow_emitsRedundantClause =
  testCase "all-var first row fires correctly; later rows are warned as unreachable"
    $ assertRight
      "redundant"
      ( runMatrixFull
          boolEnv
          [tyBool]
          occ1
          [[pvar "x" tyBool], [patTrue], [patFalse]]
          [actionA, actionB, actionC]
      )
    $ \tree warns -> do
      case tree of
        Leaf _ _ -> pure ()
        _ -> assertFailure ("expected Leaf for all-var first row, got: " ++ show tree)
      let redundantActs = [act | RedundantClause _ _ act <- warns]
      assertBool "True clause must be warned as unreachable" (actionB `elem` redundantActs)
      assertBool "False clause must be warned as unreachable" (actionC `elem` redundantActs)
      assertBool "first all-var row must not be warned as redundant" (actionA `notElem` redundantActs)
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

-- 8. Literal patterns with a variable catch-all → AtomicSwitch with Leaf default, no warnings
test_litPats_withVarDefault_noWarnings :: TestTree
test_litPats_withVarDefault_noWarnings =
  testCase "lit patterns with variable fallback -> AtomicSwitch with Leaf default, no warnings"
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
        AtomicSwitch [0] branches (Just (Leaf _ defAct)) -> do
          assertBool
            "should have literal 0 branch"
            (IntLit 0 `elem` branchLits branches)
          assertBool
            "should have literal 1 branch"
            (IntLit 1 `elem` branchLits branches)
          assertEqual "default leaf should be actionC" actionC defAct
          assertBool "should have no warnings:" (null warns)
        AtomicSwitch _ _ Nothing ->
          assertFailure "expected a default branch for the variable catch-all"
        _ ->
          assertFailure ("expected AtomicSwitch, got: " ++ show tree)

-- 9. Literal patterns without a catch-all → AtomicSwitch with Fail default + NonExhaustive
test_litPats_noDefault_nonExhaustive :: TestTree
test_litPats_noDefault_nonExhaustive =
  testCase "lit patterns without catch-all -> AtomicSwitch with Fail default and NonExhaustive"
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
        AtomicSwitch [0] branches (Just Fail) -> do
          assertBool
            "should have literal 0 branch"
            (IntLit 0 `elem` branchLits branches)
          assertBool
            "should have literal 1 branch"
            (IntLit 1 `elem` branchLits branches)
          assertBool "should emit NonExhaustive warning" (any isNonExh warns)
        _ ->
          assertFailure ("expected AtomicSwitch … (Just Fail), got: " ++ show tree)

-- 10. Constructor absent from the TypeEnv → Left error
test_unknownConstructor_isError :: TestTree
test_unknownConstructor_isError =
  testCase "pattern constructor absent from TypeEnv -> Left error" $
    assertLeft
      "unknown-con"
      (runMatrix Map.empty [tyBool] occ1 [[patTrue]] [actionA])

-- Shared data for treeToStmt and inhabitsConCol tests

varX :: Exp Id
varX = Var (Id (Name "x") tyBool)

varY :: Exp Id
varY = Var (Id (Name "y") tyBool)

-- Type environment with both List and Bool constructors
listBoolEnv :: TypeEnv
listBoolEnv = Map.union listEnv boolEnv

-- 11. treeToStmt correctly substitutes a PVar with the scrutinee expression
--
-- match y { | z => return z }
-- should compile to: return y
test_treeToStmt_singleVar_substituted :: TestTree
test_treeToStmt_singleVar_substituted =
  testCase "treeToStmt: PVar in single-column all-var row is substituted by occurrence" $ do
    let idZ = Id (Name "z") tyBool
        occMap = Map.fromList [([0 :: Int], varY)]
    r <- runFull boolEnv occMap [tyBool] [[0]] [[PVar idZ]] [[Return (Var idZ)]]
    case r of
      Left err -> assertFailure ("unexpected error: " ++ err)
      Right (stmt, _) -> assertEqual "z should be substituted with y" (Return varY) stmt

-- 12. treeToStmt correctly substitutes PVars bound through specialize and defaultMatrix
--
-- match x, y { | True, z => return z; | z, w => return z }
--
-- True branch:    z is bound to occurrence [1] (= y) → return y
-- default branch: z is bound to occurrence [0] (= x) via defaultBoundActs → return x
test_treeToStmt_multiColumn_varsBoundViaSpecialize :: TestTree
test_treeToStmt_multiColumn_varsBoundViaSpecialize =
  testCase "treeToStmt: PVars bound via specialize/defaultMatrix are correctly substituted" $ do
    let idZ = Id (Name "z") tyBool
        idW = Id (Name "w") tyBool
        occMap = Map.fromList [([0 :: Int], varX), ([1], varY)]
        matrix = [[patTrue, PVar idZ], [PVar idZ, PVar idW]]
        retZ = [Return (Var idZ)]
    r <- runFull boolEnv occMap [tyBool, tyBool] [[0], [1]] matrix [retZ, retZ]
    case r of
      Left err -> assertFailure ("unexpected error: " ++ err)
      Right (stmt, _) -> case stmt of
        Match [scrutinee] eqns -> do
          assertEqual "scrutinee should be x" varX scrutinee
          let trueBodys = [body | ([PCon k _], body) <- eqns, idName k == Name "True"]
          let defBodys = [body | ([PVar _], body) <- eqns]
          case trueBodys of
            (tb : _) -> assertEqual "True branch: z substituted with y" [Return varY] tb
            [] -> assertFailure "True branch not found in Match alts"
          case defBodys of
            (db : _) -> assertEqual "default branch: z substituted with x" [Return varX] db
            [] -> assertFailure "default branch not found in Match alts"
        _ -> assertFailure ("expected Match [x] …, got: " ++ show stmt)

-- 13. Rows after a first all-var row are warned as unreachable even when they
-- are NOT exhaustive by themselves.
--
-- match x { | z => return z; | True => return True; }
--
-- The True clause is dead code: z catches everything first.  This must be
-- warned as unreachable even though True alone does not cover the type.
test_allVar_first_shadows_nonexhaustive_rest :: TestTree
test_allVar_first_shadows_nonexhaustive_rest =
  testCase "rows after first all-var row are unreachable even if not exhaustive"
    $ assertRight
      "catchall-shadows-partial"
      ( runMatrixFull
          boolEnv
          [tyBool]
          occ1
          [[pvar "z" tyBool], [patTrue]]
          [actionA, actionB]
      )
    $ \tree warns -> do
      case tree of
        Leaf _ _ -> pure ()
        _ -> assertFailure ("expected Leaf, got: " ++ show tree)
      let redundantActs = [act | RedundantClause _ _ act <- warns]
      assertBool "True clause must be warned as unreachable" (actionB `elem` redundantActs)
      assertBool "first all-var row must not be warned" (actionA `notElem` redundantActs)

-- 14. Two-column match: (True,z) / (w,True) / (a,b)
-- The rows after the first are NOT globally redundant:
--   row 1 fires for (False, True), row 2 fires for (False, False).
-- No RedundantClause warnings should be emitted.
test_twoCol_noFalsePositive_partialOverlap :: TestTree
test_twoCol_noFalsePositive_partialOverlap =
  testCase "two-col: overlapping rows are not falsely warned as redundant"
    $ assertRight
      "two-col-no-false-pos"
      ( runMatrixFull
          boolEnv
          [tyBool, tyBool]
          occ2
          [ [patTrue, pvar "z" tyBool],
            [pvar "w" tyBool, patTrue],
            [pvar "a" tyBool, pvar "b" tyBool]
          ]
          [actionA, actionB, actionC]
      )
    $ \_ warns -> do
      let redundantActs = [act | RedundantClause _ _ act <- warns]
      assertBool "row 1 (w,True) must NOT be warned as redundant" (actionB `notElem` redundantActs)
      assertBool "row 2 (a,b) must NOT be warned as redundant" (actionC `notElem` redundantActs)
      assertBool "no RedundantClause warnings at all" (null redundantActs)

-- 15. Two-column match: genuinely redundant third row.
-- (True,True) / (False,True) / (z,True) — rows 0+1 cover all (x,True),
-- so row 2 is truly dead.
test_twoCol_genuinelyRedundant_thirdRow :: TestTree
test_twoCol_genuinelyRedundant_thirdRow =
  testCase "two-col: third row subsumed by first two is genuinely warned"
    $ assertRight
      "two-col-true-pos"
      ( runMatrixFull
          boolEnv
          [tyBool, tyBool]
          occ2
          [ [patTrue, patTrue],
            [patFalse, patTrue],
            [pvar "z" tyBool, patTrue]
          ]
          [actionA, actionB, actionC]
      )
    $ \_ warns -> do
      let redundantActs = [act | RedundantClause _ _ act <- warns]
      assertBool "row 2 (z,True) must be warned as redundant" (actionC `elem` redundantActs)
      assertBool "row 0 must not be warned" (actionA `notElem` redundantActs)
      assertBool "row 1 must not be warned" (actionB `notElem` redundantActs)

-- 16. Single-column duplicate row: second True is redundant, False is not.
test_singleCol_duplicateRow_warned :: TestTree
test_singleCol_duplicateRow_warned =
  testCase "single-col: exact duplicate constructor row is warned, later row is not"
    $ assertRight
      "dup-row"
      ( runMatrixFull
          boolEnv
          [tyBool]
          occ1
          [[patTrue], [patTrue], [patFalse]]
          [actionA, actionB, actionC]
      )
    $ \_ warns -> do
      let redundantActs = [act | RedundantClause _ _ act <- warns]
      assertBool "second True must be warned as redundant" (actionB `elem` redundantActs)
      assertBool "False must not be warned as redundant" (actionC `notElem` redundantActs)
      assertBool "first True must not be warned" (actionA `notElem` redundantActs)

-- 17. Two-column constructor match with one missing constructor:
-- the NonExhaustive witness must cover BOTH columns, not just the
-- selected constructor column.
--
-- match x, y { | True, z => ... }
-- Missing: (False, _)  — witness should be [PCon False [], PVar _]
test_twoCol_con_nonExh_witness_has_both_columns :: TestTree
test_twoCol_con_nonExh_witness_has_both_columns =
  testCase "two-col constructor: NonExhaustive witness covers both columns"
    $ assertRight
      "two-col-con-nonexh"
      ( runMatrix
          boolEnv
          [tyBool, tyBool]
          occ2
          [[patTrue, pvar "z" tyBool]]
          [actionA]
      )
    $ \_ warns -> do
      let nonExhPats = [pats | NonExhaustive _ pats <- warns]
      case nonExhPats of
        [] -> assertFailure "expected a NonExhaustive warning"
        (pats : _) -> do
          assertEqual
            "witness must have one pattern per scrutinee column (2)"
            2
            (length pats)
          case pats of
            (PCon k [] : _) ->
              assertEqual "first pattern must be the missing constructor False" (Name "False") (idName k)
            _ ->
              assertFailure ("expected PCon False as first witness pattern, got: " ++ show pats)

-- 18. Two-column literal match without a catch-all:
-- the NonExhaustive witness must cover BOTH columns, not just the
-- remaining (non-literal) column.
--
-- match n, b { | 0, z => ... }
-- Missing: (_, _)  — witness should be [PVar _, PVar _]
test_twoCol_lit_nonExh_witness_has_both_columns :: TestTree
test_twoCol_lit_nonExh_witness_has_both_columns =
  testCase "two-col literal: NonExhaustive witness covers both columns"
    $ assertRight
      "two-col-lit-nonexh"
      ( runMatrix
          boolEnv
          [tyWord, tyBool]
          occ2
          [[PLit (IntLit 0), pvar "z" tyBool]]
          [actionA]
      )
    $ \_ warns -> do
      let nonExhPats = [pats | NonExhaustive _ pats <- warns]
      case nonExhPats of
        [] -> assertFailure "expected a NonExhaustive warning"
        (pats : _) ->
          assertEqual
            "witness must have one pattern per scrutinee column (2)"
            2
            (length pats)

-- 19. inhabitsM: a matrix that has a wildcard row covering all columns is
-- exhaustive.  The bug in 'inhabitsConCol' returns a false witness when
-- mRestWit = Nothing (the wildcard rows already cover every rest-column
-- value), instead of returning Nothing (= exhaustive).
--
-- matrix: [ [True, True]   -- row 0: covers (True, True)
--          , [z,   w   ] ] -- row 1: wildcard catches everything else
-- types:  [Bool, Bool]
--
-- The wildcard row 1 means the matrix IS exhaustive → Nothing.
-- Bug: inhabitsConCol finds missingCons=[False], computes
--      mRestWit = Nothing (wildcard covers restTys), but still emits
--      Just [PCon False [], $v].
test_inhabitsM_wildcard_row_covers_all :: TestTree
test_inhabitsM_wildcard_row_covers_all =
  testCase "inhabitsM: wildcard row makes matrix exhaustive → Nothing (no false witness)" $ do
    let idZ = Id (Name "z") tyBool
        idW = Id (Name "w") tyBool
        matrix =
          [ [patTrue, patTrue],
            [PVar idZ, PVar idW]
          ]
    r <- runCompilerM boolEnv (inhabitsM matrix [tyBool, tyBool])
    case r of
      Left err -> assertFailure ("unexpected error: " ++ err)
      Right (mWit, _) ->
        assertEqual
          "wildcard row covers all patterns: inhabitsM should be Nothing"
          Nothing
          mWit

-- 20. primEnv: pairConInfo.conReturnType stores pairTy at bt (the arrow type
-- at :-> bt :-> pair at bt) instead of the result type pair at bt.
-- Fix: ConInfo [at, bt] (pair at bt).
test_pairConInfo_returnType_is_result_type :: TestTree
test_pairConInfo_returnType_is_result_type =
  testCase "pairConInfo: conReturnType should be pair at bt (result type, not arrow type)" $
    assertEqual
      "conReturnType pairConInfo"
      (pair at bt)
      (conReturnType pairConInfo)

-- 21. primEnv: inlConInfo.conReturnType stores inlTy at bt (the arrow type
-- at :-> sum at bt) instead of the result type sumTy at bt.
-- Fix: ConInfo [at] (sumTy at bt).
test_inlConInfo_returnType_is_result_type :: TestTree
test_inlConInfo_returnType_is_result_type =
  testCase "inlConInfo: conReturnType should be sumTy at bt (result type, not arrow type)" $
    assertEqual
      "conReturnType inlConInfo"
      (sumTy at bt)
      (conReturnType inlConInfo)

-- 22. primEnv: inrConInfo.conReturnType stores inlTy at bt (wrong: uses inl
-- instead of inr, and stores the arrow type rather than the result type).
-- Fix: ConInfo [bt] (sumTy at bt).
test_inrConInfo_returnType_is_result_type :: TestTree
test_inrConInfo_returnType_is_result_type =
  testCase "inrConInfo: conReturnType should be sumTy at bt (not inlTy at bt)" $
    assertEqual
      "conReturnType inrConInfo"
      (sumTy at bt)
      (conReturnType inrConInfo)

-- ---------------------------------------------------------------------------
-- Bug: `siblings \\ headCons` uses Eq Id (name + type).
--
-- `siblingConstructors` builds Id values via `idFromInfo`, which uses the
-- TypeEnv's conReturnType.  When the TypeEnv was built by `addDataTyInfo`,
-- conReturnType is polymorphic (e.g. TyCon "List" [TyVar "a"]).  But the
-- constructor Ids in the pattern matrix come from the type-checker and carry
-- concrete types (e.g. TyCon "List" [Bool]).
--
-- `siblings \\ headCons` therefore fails to remove any element of headCons
-- from siblings, so `missing` contains ALL sibling constructors instead of
-- just the absent ones.  The first element of `missing` may then be a
-- constructor that IS already covered — producing a false or wrong witness.
--
-- Fix: compare by name only:
--   let missing = filter (\s -> idName s `notElem` map idName headCons) siblings
-- ---------------------------------------------------------------------------

-- 23. polyListEnv has polymorphic return types (as addDataTyInfo produces).
-- Matrix [[Cons h t]] on [List Bool] covers only Cons; Nil is missing.
-- siblings = [Cons_poly, Nil_poly]; headCons = [Cons_concrete].
-- Bug: (\\) finds Cons_poly /= Cons_concrete → missing = [Cons_poly, Nil_poly].
-- First missing = Cons_poly → witness says "Cons missing" — WRONG.
-- Fix: missing = [Nil_poly] → witness says "Nil missing" — CORRECT.
test_nonExh_polyEnv_missingNil_witness_is_Nil :: TestTree
test_nonExh_polyEnv_missingNil_witness_is_Nil =
  testCase "poly TypeEnv: missing-constructor witness names Nil (not Cons) when Cons is covered"
    $ assertRight
      "poly-list-missing-nil"
      ( runMatrix
          polyListEnv
          [tyList]
          occ1
          [[patConsConcrete]]
          [actionA]
      )
    $ \_ warns -> do
      let nonExhPats = [pats | NonExhaustive _ pats <- warns]
      case nonExhPats of
        [] -> assertFailure "expected a NonExhaustive warning"
        (pats : _) -> case pats of
          (PCon k _ : _) ->
            assertEqual
              "missing constructor should be Nil (Cons is covered)"
              (Name "Nil")
              (idName k)
          _ ->
            assertFailure ("expected PCon as first witness pattern, got: " ++ show pats)

-- 24. Same environment, opposite case: matrix [[Nil]] covers only Nil; Cons is missing.
-- siblings = [Cons_poly, Nil_poly]; headCons = [Nil_concrete].
-- Bug: (\\) finds Nil_poly /= Nil_concrete → missing = [Cons_poly, Nil_poly].
-- First missing = Cons_poly → witness says "Cons missing" — happens to be correct
--   BY ACCIDENT (Cons is alphabetically first in Map.toList).
-- This test documents that the correct answer is Cons, and that the fix preserves it.
test_nonExh_polyEnv_missingCons_witness_is_Cons :: TestTree
test_nonExh_polyEnv_missingCons_witness_is_Cons =
  testCase "poly TypeEnv: missing-constructor witness names Cons (not Nil) when Nil is covered"
    $ assertRight
      "poly-list-missing-cons"
      ( runMatrix
          polyListEnv
          [tyList]
          occ1
          [[patNilConcrete]]
          [actionA]
      )
    $ \_ warns -> do
      let nonExhPats = [pats | NonExhaustive _ pats <- warns]
      case nonExhPats of
        [] -> assertFailure "expected a NonExhaustive warning"
        (pats : _) -> case pats of
          (PCon k _ : _) ->
            assertEqual
              "missing constructor should be Cons (Nil is covered)"
              (Name "Cons")
              (idName k)
          _ ->
            assertFailure ("expected PCon as first witness pattern, got: " ++ show pats)
