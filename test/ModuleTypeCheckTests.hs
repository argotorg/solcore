module ModuleTypeCheckTests
  ( moduleTypeCheckTests,
  )
where

import Solcore.Diagnostics (CompilerError, compilerErrorText)
import Solcore.Frontend.Module.Loader
  ( ModuleGraph (entryModule),
    ModuleTypeCheckSurface (moduleSurfaceImportedDecls),
    loadModuleGraph,
    moduleLocalTypeCheckSurface,
  )
import Solcore.Frontend.Parser.SolcoreParser (parseCompUnit)
import Solcore.Frontend.Pretty.TreePretty qualified as TreePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.Syntax.NameResolution (nameResolution)
import Solcore.Frontend.Syntax.SyntaxTree qualified as Source
import Solcore.Frontend.TypeInference.Id (Id)
import Solcore.Frontend.TypeInference.TcContract
  ( TopDeclCheck (..),
    TopDeclCheckMode (CheckTopDeclBody),
    typeInferTopDeclChecks,
  )
import Solcore.Frontend.TypeInference.TcEnv (TcEnv)
import Solcore.Frontend.TypeInference.TcModule
import Solcore.Pipeline.Options (stdOpt)
import Test.Tasty
import Test.Tasty.HUnit

moduleTypeCheckTests :: TestTree
moduleTypeCheckTests =
  testGroup
    "Module typecheck"
    [ testCase "retagged generated declarations default to local" $ do
        let generated = singleDecl (retagModuleInferenceDecls [] [funDecl "generated"])
        assertEqual
          "generated decl segment"
          ModuleLocalDecl
          (moduleInferenceDeclSegment generated),
      testCase "retagged mixed mutual declarations prefer local segment" $ do
        let inferenceDecls =
              [ ModuleInferenceDecl ModuleImportedDecl (funDecl "imported"),
                ModuleInferenceDecl ModuleLocalDecl (funDecl "local")
              ]
            retagged =
              singleDecl $
                retagModuleInferenceDecls
                  inferenceDecls
                  [TMutualDef [funDecl "imported", funDecl "local"]]
        assertEqual
          "mixed mutual segment"
          ModuleLocalDecl
          (moduleInferenceDeclSegment retagged),
      testCase "resolved input derives initial inference segments" $ do
        let inferenceDecls =
              [ ModuleInferenceDecl ModuleQualifiedDecl (funDecl "qualified"),
                ModuleInferenceDecl ModuleLocalDecl (funDecl "local"),
                ModuleInferenceDecl ModuleImportedDecl (funDecl "imported")
              ]
        assertEqual
          "initial inference segments"
          (map moduleInferenceDeclSegment inferenceDecls)
          (map moduleInferenceDeclSegment (moduleInitialInferenceDecls (resolvedModuleInput inferenceDecls))),
      testCase "type inference trusts imported bodies while checking local bodies" $ do
        result <-
          typeInferModuleLocals
            stdOpt
            (moduleInput [ModuleInferenceDecl ModuleImportedDecl badImportedFun, ModuleInferenceDecl ModuleLocalDecl usesImportedFun])
        assertRight "imported body should be trusted" result,
      testCase "type inference checks local bodies" $ do
        result <-
          typeInferModuleLocals
            stdOpt
            (moduleInput [ModuleInferenceDecl ModuleLocalDecl badImportedFun])
        assertLeft "local body should be checked" result,
      testCase "numeric fixed-array size survives resolution and kind checking" $ do
        parsedResult <-
          parseCompUnit $
            unlines
              [ "enum array<element> { array }",
                "function accept(xs: word[4]) returns (()) {",
                "  return;",
                "}"
              ]
        parsed <-
          case parsedResult of
            Left err -> assertFailure ("unexpected parse failure:\n" ++ err)
            Right compUnit -> pure compUnit
        resolvedResult <- nameResolution parsed
        case resolvedResult of
          Left err ->
            assertFailure
              ("unexpected name-resolution failure:\n" ++ compilerErrorText err)
          Right (CompUnit resolvedImports resolvedDecls) -> do
            checked <-
              typeInferTopDeclChecks
                stdOpt
                resolvedImports
                []
                []
                [ TopDeclCheck CheckTopDeclBody decl
                | decl <- resolvedDecls
                ]
            assertRight
              "numeric fixed-array size should be kind-correct"
              checked,
      testCase "interface signature has no body to typecheck" $ do
        checked <-
          typecheckSource $
            unlines
              [ "interface Reader {",
                "  function read(key: word) external view returns (word);",
                "}"
              ]
        case checked of
          Left err ->
            assertFailure
              ("interface signature should typecheck:\n" ++ compilerErrorText err)
          Right (CompUnit _ [TContr (Contract _ _ [CSignatureDecl isExternal sig])], _) -> do
            assertBool "external visibility is preserved" isExternal
            assertEqual "signature name" (Name "read") (sigName sig)
            assertEqual "signature return" (Just wordTy) (sigReturn sig)
          Right other ->
            assertFailure ("unexpected typed interface shape: " ++ show (fst other)),
      testCase "ordinary contract function still checks its empty body" $ do
        checked <-
          typecheckSource $
            unlines
              [ "contract Reader {",
                "  function read(key: word) external view returns (word) {}",
                "}"
              ]
        assertLeft "non-unit contract function with an empty body" checked,
      testCase "selective struct import preserves source metadata and pretty round-trips" $ do
        graphResult <-
          loadModuleGraph
            "test/imports"
            Nothing
            []
            "test/imports/struct_metadata_main.solc"
        graph <-
          case graphResult of
            Left err -> assertFailure ("unexpected module load failure:\n" ++ err)
            Right loadedGraph -> pure loadedGraph
        surface <-
          case moduleLocalTypeCheckSurface graph (entryModule graph) of
            Left err -> assertFailure ("unexpected module surface failure:\n" ++ err)
            Right loadedSurface -> pure loadedSurface
        importedStruct <-
          case
              [ dt
              | Source.TDataDef dt <- moduleSurfaceImportedDecls surface,
                Source.dataName dt == "RenamedPair"
              ] of
            [dt] -> pure dt
            unexpectedDecls ->
              assertFailure
                ("expected one imported RenamedPair declaration, got " ++ show unexpectedDecls)
        assertEqual
          "renamed struct retains kind, field names, and field types"
          (Source.StructTy "RenamedPair" [] ["left", "right"] [Source.TyCon "word" [], Source.TyCon "bool" []])
          importedStruct
        let rendered = TreePretty.pretty (Source.TDataDef importedStruct)
        reparsed <- parseCompUnit rendered
        case reparsed of
          Left err -> assertFailure ("pretty-printed imported struct did not parse:\n" ++ err)
          Right unit ->
            assertEqual
              ("round trip: " ++ rendered)
              (Source.CompUnit [] [Source.TDataDef importedStruct])
              unit
    ]

assertRight :: String -> Either CompilerError a -> Assertion
assertRight _ (Right _) = pure ()
assertRight label (Left err) =
  assertFailure (label ++ ": unexpected failure:\n" ++ compilerErrorText err)

assertLeft :: String -> Either CompilerError a -> Assertion
assertLeft _ (Left _) = pure ()
assertLeft label (Right _) =
  assertFailure (label ++ ": expected failure")

typecheckSource :: String -> IO (Either CompilerError (CompUnit Id, TcEnv))
typecheckSource source = do
  parsedResult <- parseCompUnit source
  parsed <-
    case parsedResult of
      Left err -> assertFailure ("unexpected parse failure:\n" ++ err)
      Right compUnit -> pure compUnit
  resolvedResult <- nameResolution parsed
  case resolvedResult of
    Left err -> pure (Left err)
    Right (CompUnit resolvedImports resolvedDecls) ->
      typeInferTopDeclChecks
        stdOpt
        resolvedImports
        []
        []
        [ TopDeclCheck CheckTopDeclBody decl
        | decl <- resolvedDecls
        ]

moduleInput :: [ModuleInferenceDecl] -> ModuleTypeCheckInput
moduleInput inferenceDecls =
  withPreparedModuleInferenceDecls (resolvedModuleInput inferenceDecls) inferenceDecls

resolvedModuleInput :: [ModuleInferenceDecl] -> ModuleResolvedTypeCheckInput
resolvedModuleInput inferenceDecls =
  ModuleResolvedTypeCheckInput
    { moduleResolvedInputImports = [],
      moduleResolvedInputQualifiedDecls = declsInSegment ModuleQualifiedDecl inferenceDecls,
      moduleResolvedInputLocalDecls = declsInSegment ModuleLocalDecl inferenceDecls,
      moduleResolvedInputImportedDecls = declsInSegment ModuleImportedDecl inferenceDecls,
      moduleResolvedInputTrustedInstanceHeads = [],
      moduleResolvedInputPartialImportedTypes = []
    }

declsInSegment :: ModuleDeclSegment -> [ModuleInferenceDecl] -> [TopDecl Name]
declsInSegment segment =
  map moduleInferenceDeclTopDecl
    . filter ((== segment) . moduleInferenceDeclSegment)

singleDecl :: [ModuleInferenceDecl] -> ModuleInferenceDecl
singleDecl [decl] = decl
singleDecl inferenceDecls =
  error ("expected exactly one module inference declaration, got " ++ show (length inferenceDecls))

funDecl :: String -> TopDecl Name
funDecl funName =
  TFunDef
    FunDef
      { funIsPublic = False,
        funSignature =
          wordSignature funName,
        funDefBody = [Return (Lit (IntLit 0))]
      }

wordTy :: Ty
wordTy =
  TyCon (Name "word") []

badImportedFun :: TopDecl Name
badImportedFun =
  TFunDef
    FunDef
      { funIsPublic = False,
        funSignature = wordSignature "badImported",
        funDefBody = [Return (Var (Name "missing"))]
      }

usesImportedFun :: TopDecl Name
usesImportedFun =
  TFunDef
    FunDef
      { funIsPublic = False,
        funSignature = wordSignature "usesImported",
        funDefBody = [Return (Call Nothing (Name "badImported") [])]
      }

wordSignature :: String -> Signature Name
wordSignature funName =
  Signature
    { sigVars = [],
      sigContext = [],
      sigName = Name funName,
      sigParams = [],
      sigRetComptime = False,
      sigReturn = Just wordTy,
      sigPayable = False
    }
