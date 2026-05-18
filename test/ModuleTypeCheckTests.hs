module ModuleTypeCheckTests
  ( moduleTypeCheckTests,
  )
where

import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcModule
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
          (moduleInferenceDeclSegment retagged)
    ]

singleDecl :: [ModuleInferenceDecl] -> ModuleInferenceDecl
singleDecl [decl] = decl
singleDecl inferenceDecls =
  error ("expected exactly one module inference declaration, got " ++ show (length inferenceDecls))

funDecl :: String -> TopDecl Name
funDecl funName =
  TFunDef
    FunDef
      { funSignature =
          Signature
            { sigVars = [],
              sigContext = [],
              sigName = Name funName,
              sigParams = [],
              sigReturn = Just wordTy
            },
        funDefBody = [Return (Lit (IntLit 0))]
      }

wordTy :: Ty
wordTy =
  TyCon (Name "word") []
