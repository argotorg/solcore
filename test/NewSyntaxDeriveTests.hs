module NewSyntaxDeriveTests (newSyntaxDeriveTests) where

import Data.List (isInfixOf)
import Solcore.Desugarer.DeriveClass (deriveClassTopDecls)
import Solcore.Frontend.Syntax
import Solcore.Pipeline.Options (Option (..), emptyOption)
import Solcore.Pipeline.SolcorePipeline (compile)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

newSyntaxDeriveTests :: TestTree
newSyntaxDeriveTests =
  testGroup
    "new syntax trait derivation"
    [ testCase "imported traits and aliased marker derive through specialization" $
        compilesFixture "main.sol",
      testCase "qualified imported trait derives through specialization" $
        compilesFixture "qualified.sol",
      testCase "namespace alias qualifies a derived trait" $
        compilesFixture "qualified-alias.sol",
      testCase "private qualified trait cannot be derived" $
        rejectsFixture "private.sol" "unknown trait",
      testCase "unknown qualifier cannot fall back to a visible trait" $
        rejectsFixture "unknown.sol" "unknown trait",
      testCase "marker trait derives without Generic" $ do
        let dt = derivingData markerName "Empty" [] []
        result <- generated dt [TClassDef marker]
        assertEqual "one empty instance" [Instance False [] [] markerName [] (targetTy dt) []] result,
      testCase "phantom parameters require the derived trait" $ do
        let dt = derivingData markerName "Phantom" [a] [Constr "Phantom" [word]]
        result <- generated dt [TClassDef marker]
        assertEqual "parameter constraint" [[InCls markerName (TyVar a) []]] (map instContext result),
      testCase "duplicate requests reach the overlap checker" $ do
        let dt = (derivingData markerName "Empty" [] []) {dataDerives = [markerName, markerName]}
        result <- generated dt [TClassDef marker]
        assertEqual "both instances remain" 2 (length result),
      testCase "unknown trait reports its target" $ do
        let dt = derivingData "Missing" "Box" [] []
        failsWith "unknown trait" (deriveClassTopDecls [dt] []),
      testCase "multi-parameter trait is rejected" $ do
        let cls = Class [a, b] [] "Convert" [b] a []
            dt = derivingData "Convert" "Box" [] []
        failsWith "only single-parameter traits" (deriveClassTopDecls [dt] [TClassDef cls]),
      testCase "generic contract enum is rejected" $ do
        let dt = derivingData markerName (QualName "C" "Local") [] []
            contract = TContr (Contract "C" [a] [CDataDecl dt])
        failsWith "generic contract parameters" (deriveClassTopDecls [dt] [TClassDef marker, contract]),
      testCase "Self parameters and results use the manual Generic representation" $ do
        let dt = derivingData "Clone" "Box" [] [Constr "Box" [bool]]
            signature = Signature [] [] "clone" [Typed False "x" (TyVar a)] False (Just (TyVar a)) False
            cls = Class [a] [] "Clone" [] a [signature]
            generic = genericFor dt word
        [inst] <- generated dt [TClassDef cls, TInstDef generic]
        [FunDef _ sig body] <- pure (instFunctions inst)
        let from = TyExp (method "Generic" "from" [Var "x"]) word
            delegated = TyExp (method "Clone" "clone" [from]) word
        assertEqual "nominal input" [Typed False "x" (targetTy dt)] (sigParams sig)
        assertEqual "nominal output" (Just (targetTy dt)) (sigReturn sig)
        assertEqual "conversion sequence" [Return (method "Generic" "to" [delegated])] body,
      testCase "only Self arguments are converted" $ do
        let dt = derivingData "Compare" "Box" [] [Constr "Box" [word]]
            signature = Signature [] [] "check" [Typed False "x" (TyVar a), Typed False "flag" bool] False (Just bool) False
            cls = Class [a] [] "Compare" [] a [signature]
        [inst] <- generated dt [TClassDef cls, TInstDef (genericFor dt word)]
        [FunDef _ _ body] <- pure (instFunctions inst)
        let from = TyExp (method "Generic" "from" [Var "x"]) word
        assertEqual "bool passes through" [Return (TyExp (method "Compare" "check" [from, Var "flag"]) bool)] body,
      testCase "method binders remain independent of enum binders" $ do
        let dt = derivingData "Choose" "Box" [b] [Constr "Box" [word]]
            signature = Signature [b] [] "choose" [Typed False "value" (TyVar b), Typed False "witness" (TyVar a)] False (Just (TyVar b)) False
            cls = Class [a] [] "Choose" [] a [signature]
        [inst] <- generated dt [TClassDef cls, TInstDef (genericFor dt word)]
        [FunDef _ sig _] <- pure (instFunctions inst)
        case sigParams sig of
          [Typed _ _ methodTy, Typed _ _ selfTy] -> do
            assertBool "method type is fresh" (methodTy /= TyVar b)
            assertEqual "nominal Self keeps enum parameter" (targetTy dt) selfTy
            assertEqual "result uses method parameter" (Just methodTy) (sigReturn sig)
          other -> assertFailure (show other),
      testCase "method equality constraints substitute Self and fresh method binders" $ do
        let dt = derivingData "Choose" "Box" [b] [Constr "Box" [word]]
            signature = Signature [b] [TyVar b :~: TyVar a] "choose" [Typed False "value" (TyVar b), Typed False "witness" (TyVar a)] False (Just (TyVar b)) False
            cls = Class [a] [] "Choose" [] a [signature]
        [inst] <- generated dt [TClassDef cls, TInstDef (genericFor dt word)]
        [FunDef _ sig _] <- pure (instFunctions inst)
        case sigVars sig of
          [methodVariable] -> do
            assertBool "method binder is fresh" (methodVariable /= b)
            assertEqual "equality is preserved and both sides substituted" [TyVar methodVariable :~: targetTy dt] (sigContext sig)
          other -> assertFailure (show other),
      testCase "nested Self is diagnosed" $ do
        let dt = derivingData "Nested" "Box" [] [Constr "Box" [word]]
            signature = Signature [] [] "inspect" [Typed False "x" (TyCon "pair" [TyVar a, word])] False (Just bool) False
            cls = Class [a] [] "Nested" [] a [signature]
        failsWith "not inside another type" (deriveClassTopDecls [dt] [TClassDef cls, TInstDef (genericFor dt word)]),
      testCase "empty enum argument is eliminated by empty match" $ do
        let dt = derivingData "Clone" "Never" [] []
            signature = Signature [] [] "clone" [Typed False "x" (TyVar a)] False (Just (TyVar a)) False
            cls = Class [a] [] "Clone" [] a [signature]
        [inst] <- generated dt [TClassDef cls]
        assertEqual "unreachable body" [[Match [Var "x"] []]] (map funBody (instFunctions inst)),
      testCase "empty enum argument uses in-scope absurd before empty match" $ do
        let dt = derivingData "Clone" "Never" [] []
            signature = Signature [] [] "clone" [Typed False "x" (TyVar a)] False (Just (TyVar a)) False
            cls = Class [a] [] "Clone" [] a [signature]
            absurd = TFunDef (FunDef False (Signature [a] [] "absurd" [] False (Just (TyVar a)) False) [])
        [inst] <- generated dt [TClassDef cls, absurd]
        assertEqual "absurd body" [[Return (Call Nothing "absurd" [])]] (map funBody (instFunctions inst)),
      testCase "empty factory can delegate to absurd" $ do
        let dt = derivingData "Make" "Never" [] []
            signature = Signature [] [] "make" [] False (Just (TyVar a)) False
            cls = Class [a] [] "Make" [] a [signature]
            absurd = TFunDef (FunDef False (Signature [a] [] "absurd" [] False (Just (TyVar a)) False) [])
        [inst] <- generated dt [TClassDef cls, absurd]
        assertEqual "absurd body" [[Return (Call Nothing "absurd" [])]] (map funBody (instFunctions inst)),
      testCase "missing Generic produces an explicit diagnostic" $ do
        let dt = derivingData "Clone" "Box" [] [Constr "Box" [word]]
            signature = Signature [] [] "clone" [Typed False "x" (TyVar a)] False (Just (TyVar a)) False
            cls = Class [a] [] "Clone" [] a [signature]
        failsWith "no Generic instance" (deriveClassTopDecls [dt] [TClassDef cls]),
      testCase "qualified trait target is preserved" $ do
        let target = QualName "support" "Marker"
            dt = derivingData target "Box" [] []
            cls = marker {className = target}
        result <- generated dt [TClassDef cls]
        assertEqual "qualified instance head" [target] (map instName result)
    ]
  where
    a = TVar "a"
    b = TVar "b"
    word = TyCon "word" []
    bool = TyCon "bool" []
    markerName = "Marker"
    marker = Class [a] [] markerName [] a []

-- All callers provide exactly one local enum. Returning only appended
-- instances makes these assertions independent of the surrounding declarations.
generated :: DataTy -> [TopDecl Name] -> IO [Instance Name]
generated dt declarations =
  case deriveClassTopDecls [dt] declarations of
    Left err -> assertFailure err >> pure []
    Right result -> pure [inst | TInstDef inst <- drop (length declarations) result]

derivingData :: Name -> Name -> [Tyvar] -> [Constr] -> DataTy
derivingData target enumName parameters constructors =
  (DataTy enumName parameters constructors) {dataDerives = [target]}

targetTy :: DataTy -> Ty
targetTy dt = TyCon (dataName dt) (map TyVar (dataParams dt))

genericFor :: DataTy -> Ty -> Instance Name
genericFor dt representation =
  Instance False (dataParams dt) [] "Generic" [representation] (targetTy dt) []

method :: Name -> String -> [Exp Name] -> Exp Name
method cls fun = Call Nothing (QualName cls fun)

funBody :: FunDef Name -> Body Name
funBody (FunDef _ _ body) = body

failsWith :: (Show a) => String -> Either String a -> Assertion
failsWith fragment result = case result of
  Left err -> assertBool err (fragment `isInfixOf` err)
  Right value -> assertFailure ("Expected failure, got " ++ show value)

compilesFixture :: FilePath -> Assertion
compilesFixture file = do
  let folder = "test/new-syntax/derive"
      options = (emptyOption (folder </> file)) {optRootDir = folder, optNoGenDispatch = True}
  result <- compile options
  case result of
    Left err -> assertFailure err
    Right _ -> pure ()

rejectsFixture :: FilePath -> String -> Assertion
rejectsFixture file fragment = do
  let folder = "test/new-syntax/derive"
      options = (emptyOption (folder </> file)) {optRootDir = folder, optNoGenDispatch = True}
  result <- compile options
  case result of
    Left err -> assertBool err (fragment `isInfixOf` err)
    Right _ -> assertFailure "Expected derivation to reject the inaccessible trait"
