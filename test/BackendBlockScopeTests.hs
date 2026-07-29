module BackendBlockScopeTests (backendBlockScopeTests) where

import Language.Hull qualified as Hull
import Language.Yul (YLiteral (..), YulExp (..), YulStmt (..))
import Solcore.Backend.Mast
import Solcore.Backend.MastEval (defaultFuel, evalCompUnit)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt (Literal (..))
import Solcore.Pipeline.Options (Option (..), stdOpt)
import Solcore.Pipeline.SolcorePipeline (compile)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

backendBlockScopeTests :: TestTree
backendBlockScopeTests =
  testGroup
    "Backend lexical blocks"
    [ sourceBlockTests,
      typeRegistryScopeTest,
      declarationPointScopeTest,
      loopBlockScopeTest
    ]

sourceBlockTests :: TestTree
sourceBlockTests =
  testCase "blocks preserve shadowing, outer updates, and inline evaluation" $ do
    let folder = "./test/examples/cases"
        path = folder </> "block-shadowing.solc"
    result <-
      compile
        stdOpt
          { fileName = path,
            optRootDir = folder,
            optNoGenDispatch = True
          }
    case result of
      Left err -> assertFailure err
      Right objects -> assertMain objects

assertMain :: [Hull.Object] -> Assertion
assertMain objects =
  case [ body
       | Hull.Object name code _ <- objects,
         name == "BlockShadowing",
         Hull.SFunction "main" _ _ body <- code
       ] of
    [body] -> do
      [value | Hull.SReturn (Hull.EWord value) <- body]
        @?= [3]
      assertBinding body "directShadow" 1
      assertBinding body "inlineShadow" 1
      assertBinding body "inlineUpdate" 3
      assertBinding body "updateBeforeShadow" 2
      mapM_
        ( \value ->
            assertBool
              ("expected assignment " ++ show value ++ " in a nested Hull block")
              (any (blockAssigns value) body)
        )
        [2, 3]
    bodies ->
      assertFailure
        ("expected one BlockShadowing main body, got " ++ show (length bodies))

assertBinding :: [Hull.Stmt] -> Hull.Name -> Integer -> Assertion
assertBinding body name expected =
  [value | Hull.SAssign (Hull.EVar actual) (Hull.EWord value) <- body, actual == name]
    @?= [expected]

blockAssigns :: Integer -> Hull.Stmt -> Bool
blockAssigns expected (Hull.SBlock body) =
  any isExpectedAssignment body
  where
    isExpectedAssignment
      (Hull.SAssign (Hull.EVar "x") (Hull.EWord value)) =
        value == expected
    isExpectedAssignment _ = False
blockAssigns _ _ = False

typeRegistryScopeTest :: TestTree
typeRegistryScopeTest =
  testCase "assembly uses and updates the block-local identity" $ do
    evaluatedReturn typeRegistryUnit @?= wordLiteral 1
    case [ rhs
         | MastSeq body <- evaluatedBody typeRegistryUnit,
           MastAsm [YAssign [Name "x"] rhs] <- body
         ] of
      [rhs] -> rhs @?= YLit (YulNumber 2)
      result -> assertFailure ("unexpected evaluated assembly: " ++ show result)

typeRegistryUnit :: MastCompUnit
typeRegistryUnit =
  singleFunctionUnit
    [ MastLet False outerId (Just outerType) (Just (wordLiteral 1)),
      MastSeq
        [ MastLet False innerId (Just wordType) (Just (wordLiteral 2)),
          MastAsm [YAssign [Name "x"] (YIdent (Name "x"))]
        ],
      MastReturn (MastVar outerId)
    ]
  where
    -- Keep the outer type ordered after "word" so a name-only Map collapse
    -- would incorrectly prefer its known value.
    outerType = MastTyCon (Name "zzOuter") []
    wordType = MastTyCon (Name "word") []
    outerId = MastId (Name "x") outerType
    innerId = MastId (Name "x") wordType

declarationPointScopeTest :: TestTree
declarationPointScopeTest =
  testCase "shadow restoration keeps updates made before the declaration" $
    evaluatedReturn declarationPointUnit @?= wordLiteral 2

declarationPointUnit :: MastCompUnit
declarationPointUnit =
  singleFunctionUnit
    [ MastLet False xId (Just wordType) (Just (wordLiteral 1)),
      MastSeq
        [ MastAssign xId (wordLiteral 2),
          MastLet False xId (Just wordType) (Just (wordLiteral 3))
        ],
      MastReturn (MastVar xId)
    ]
  where
    wordType = MastTyCon (Name "word") []
    xId = MastId (Name "x") wordType

loopBlockScopeTest :: TestTree
loopBlockScopeTest =
  testCase "loop-local block shadowing does not change following loop statements" $
    case evaluatedBody loopScopeUnit of
      [ _,
        MastFor
          _
          _
          _
          [MastSeq _, MastAssign _ (MastLit (IntLit value))],
        _
        ] ->
          value @?= 1
      body -> assertFailure ("unexpected evaluated loop body: " ++ show body)

loopScopeUnit :: MastCompUnit
loopScopeUnit =
  singleFunctionUnit
    [ MastLet False outerId (Just wordType) (Just (wordLiteral 1)),
      MastFor
        (MastSeq [])
        (wordLiteral 1)
        (MastSeq [])
        [ MastSeq
            [MastLet False outerId (Just wordType) (Just (wordLiteral 2))],
          MastAssign resultId (MastVar outerId)
        ],
      MastReturn (MastVar outerId)
    ]
  where
    wordType = MastTyCon (Name "word") []
    outerId = MastId (Name "x") wordType
    resultId = MastId (Name "result") wordType

singleFunctionUnit :: [MastStmt] -> MastCompUnit
singleFunctionUnit body =
  MastCompUnit
    []
    [ MastTContr
        ( MastContract
            (Name "C")
            [ MastCFunDecl
                MastFunDef
                  { mastFunName = Name "main",
                    mastFunParams = [],
                    mastFunRetComptime = False,
                    mastFunReturn = MastTyCon (Name "word") [],
                    mastFunBody = body
                  }
            ]
        )
    ]

evaluatedReturn :: MastCompUnit -> MastExp
evaluatedReturn unit =
  case reverse (evaluatedBody unit) of
    MastReturn value : _ -> value
    body -> error ("missing evaluated return: " ++ show body)

evaluatedBody :: MastCompUnit -> [MastStmt]
evaluatedBody unit =
  case evalCompUnit defaultFuel unit of
    ( MastCompUnit
        _
        [MastTContr (MastContract _ (MastCFunDecl function : _))],
      _
      ) ->
        mastFunBody function
    result -> error ("unexpected evaluated unit: " ++ show result)

wordLiteral :: Integer -> MastExp
wordLiteral = MastLit . IntLit
