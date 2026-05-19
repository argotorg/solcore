module ModuleTypeCheckTests
  ( moduleTypeCheckTests,
  )
where

import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcModule
import Solcore.Pipeline.Options (noDesugarOpt)
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
      testCase "type inference trusts imported bodies while checking local bodies" $ do
        result <-
          typeInferModuleLocals
            noDesugarOpt
            (moduleInput [ModuleInferenceDecl ModuleImportedDecl badImportedFun, ModuleInferenceDecl ModuleLocalDecl usesImportedFun])
        assertRight "imported body should be trusted" result,
      testCase "type inference checks local bodies" $ do
        result <-
          typeInferModuleLocals
            noDesugarOpt
            (moduleInput [ModuleInferenceDecl ModuleLocalDecl badImportedFun])
        assertLeft "local body should be checked" result
    ]

assertRight :: String -> Either String a -> Assertion
assertRight _ (Right _) = pure ()
assertRight label (Left err) =
  assertFailure (label ++ ": unexpected failure:\n" ++ err)

assertLeft :: String -> Either String a -> Assertion
assertLeft _ (Left _) = pure ()
assertLeft label (Right _) =
  assertFailure (label ++ ": expected failure")

moduleInput :: [ModuleInferenceDecl] -> ModuleTypeCheckInput
moduleInput inferenceDecls =
  ModuleTypeCheckInput
    { moduleInferenceDecls = inferenceDecls,
      moduleResolvedImports = [],
      moduleResolvedQualifiedDecls = declsInSegment ModuleQualifiedDecl inferenceDecls,
      moduleResolvedLocalDecls = declsInSegment ModuleLocalDecl inferenceDecls,
      moduleResolvedImportedDecls = declsInSegment ModuleImportedDecl inferenceDecls,
      moduleTrustedInstanceHeads = [],
      modulePartialImportedTypes = []
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
      { funSignature =
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
      { funSignature = wordSignature "badImported",
        funDefBody = [Return (Var (Name "missing"))]
      }

usesImportedFun :: TopDecl Name
usesImportedFun =
  TFunDef
    FunDef
      { funSignature = wordSignature "usesImported",
        funDefBody = [Return (Call Nothing (Name "badImported") [])]
      }

wordSignature :: String -> Signature Name
wordSignature funName =
  Signature
    { sigVars = [],
      sigContext = [],
      sigName = Name funName,
      sigParams = [],
      sigReturn = Just wordTy
    }
