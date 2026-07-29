module BackendNameEncodingTests (backendNameEncodingTests) where

import Common.LightYear (runParserE)
import Control.Monad (forM_)
import Data.List (nub)
import Language.Hull qualified as Hull
import Language.Hull.Parser (hullObject)
import Solcore.Backend.EmitHull (emitHull)
import Solcore.Backend.Mast
import Solcore.Backend.NameEncoding
import Solcore.Frontend.Syntax.Contract (Constr (..), DataTy (..))
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Ty
import Test.Tasty
import Test.Tasty.HUnit

backendNameEncodingTests :: TestTree
backendNameEncodingTests =
  testGroup
    "Backend name encoding"
    [ sourceNameTests,
      typeIdentityTests,
      specialisedNameTests,
      hullTypeNameTest
    ]

sourceNameTests :: TestTree
sourceNameTests =
  testGroup
    "source names"
    [ testCase "plain backend names retain their spelling" $
        encodeBackendName (Name "main") @?= "main",
      testCase "qualification cannot collide with underscore spelling" $ do
        let names =
              [ QualName (QualName (Name "A") "B") "C",
                QualName (Name "A_B") "C",
                QualName (Name "A") "B_C",
                Name "A_B_C"
              ]
        assertDistinct (map encodeBackendName names),
      testCase "plain names cannot forge the reserved namespace" $ do
        let plain = Name "$$Qforged"
            qualified = QualName (Name "Q") "forged"
        assertBool
          "reserved plain and qualified names must differ"
          (encodeBackendName plain /= encodeBackendName qualified)
    ]

typeIdentityTests :: TestTree
typeIdentityTests =
  testGroup
    "type identities"
    [ testCase "constructor arity and argument boundaries remain distinct" $ do
        let oneArgument =
              TyCon (Name "Container") [TyCon (Name "A_B") []]
            twoArguments =
              TyCon
                (Name "Container")
                [TyCon (Name "A") [], TyCon (Name "B") []]
        assertBool
          "one structured argument must not equal two arguments"
          (encodeTypeIdentity oneArgument /= encodeTypeIdentity twoArguments),
      testCase "qualified and underscore-spelled constructors remain distinct" $ do
        let qualified = TyCon (QualName (Name "C") "S") []
            flat = TyCon (Name "C_S") []
        assertBool
          "C.S and C_S must have distinct type identities"
          (encodeTypeIdentity qualified /= encodeTypeIdentity flat),
      testCase "builtin unit and a source type named unit remain distinct" $ do
        let builtinUnit = TyCon (Name "()") []
            sourceUnit = TyCon (Name "unit") []
        assertBool
          "() and unit must have distinct type identities"
          (encodeTypeIdentity builtinUnit /= encodeTypeIdentity sourceUnit),
      testCase "type representation constructors remain distinct" $ do
        let name = Name "T"
            variants =
              [ TyCon name [],
                TyVar (TVar name),
                TyVar (Skolem name),
                Meta (MetaTv name)
              ]
        assertDistinct (map encodeTypeIdentity variants)
    ]

specialisedNameTests :: TestTree
specialisedNameTests =
  testGroup
    "specialised names"
    [ testCase "source identity survives specialisation" $ do
        let ty = TyCon (Name "word") []
            qualified = encodeSpecialisedName (QualName (Name "C") "f") [ty]
            flat = encodeSpecialisedName (Name "C_f") [ty]
        assertBool
          "C.f and C_f specialisations must differ"
          (qualified /= flat),
      testCase "unspecialised and specialised declarations are disjoint" $ do
        let name = Name "f"
            ty = TyCon (Name "word") []
        assertBool
          "a specialised name must not equal its unspecialised source name"
          (encodeSpecialisedName name [] /= encodeSpecialisedName name [ty])
    ]

hullTypeNameTest :: TestTree
hullTypeNameTest =
  testCase "Hull type labels use encoded source identities" $ do
    objects <- emitHull False mastCompUnit
    case objects of
      [Hull.Object _ statements _] -> do
        functionArgumentLabels statements
          @?= [ ("qualifiedValue", encodeBackendName qualifiedType),
                ("flatValue", encodeBackendName flatType)
              ]
        forM_ objects $ \object ->
          case runParserE hullObject "<generated Hull>" (show object) of
            Left err ->
              assertFailure ("generated Hull must parse successfully:\n" ++ err)
            Right _ -> pure ()
      _ -> assertFailure ("unexpected Hull objects: " ++ show objects)
  where
    qualifiedType = QualName (Name "C") "S"
    flatType = Name "C_S"

    mastCompUnit =
      MastCompUnit
        []
        [ MastTContr
            ( MastContract
                (Name "C")
                [ MastCDataDecl (nullaryData qualifiedType),
                  MastCDataDecl (nullaryData flatType),
                  identityFunction "qualifiedValue" qualifiedType,
                  identityFunction "flatValue" flatType
                ]
            )
        ]

    nullaryData name =
      DataTy
        { dataName = name,
          dataParams = [],
          dataConstrs = [Constr (QualName name "Value") []]
        }

    identityFunction functionName typeName =
      MastCFunDecl
        MastFunDef
          { mastFunName = functionName,
            mastFunParams = [MastParam "value" False mastType],
            mastFunRetComptime = False,
            mastFunReturn = mastType,
            mastFunBody = [MastReturn (MastVar (MastId "value" mastType))]
          }
      where
        mastType = MastTyCon typeName []

functionArgumentLabels :: [Hull.Stmt] -> [(Hull.Name, String)]
functionArgumentLabels statements =
  [ (functionName, label)
  | Hull.SFunction functionName [Hull.TArg _ (Hull.TNamed label _)] _ _ <- statements
  ]

assertDistinct :: (Eq a, Show a) => [a] -> Assertion
assertDistinct values =
  assertEqual
    ("expected pairwise-distinct values, got " ++ show values)
    (length values)
    (length (nub values))
