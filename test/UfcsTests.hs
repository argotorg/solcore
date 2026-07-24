{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module UfcsTests (ufcsTests) where

import Data.Data (Data)
import Data.Generics (everything, mkQ)
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import Solcore.Desugarer.FieldAccess (fieldDesugarTopDecls)
import Solcore.Desugarer.IndirectCall (indirectCallTopDecls)
import Solcore.Diagnostics (compilerErrorText)
import Solcore.Frontend.Parser.SolcoreParser (parseCompUnit)
import Solcore.Frontend.Syntax
import Solcore.Frontend.Syntax.NameResolution (nameResolution)
import Solcore.Frontend.Syntax.SyntaxTree qualified as Surface
import Solcore.Pipeline.Options (Option (..), emptyOption)
import Solcore.Pipeline.SolcorePipeline (compile)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

ufcsTests :: TestTree
ufcsTests =
  testGroup
    "Value-receiver UFCS"
    [ testCase "rewrites parameters, locals, indexed values, and computed values" $ do
        resolved <- resolveSource valueReceiverSource
        let receiverArgs =
              mapMaybe firstArgument (callsNamed receiverMethodName resolved)
        length receiverArgs @?= 4
        assertBool "parameter receiver was not prepended" (any (isVar "param") receiverArgs)
        assertBool "local receiver was not prepended" (any (isVar "local") receiverArgs)
        assertBool "indexed receiver was not prepended" (any isIndexedReceiver receiverArgs)
        assertBool "computed receiver was not prepended" (any isComputedReceiver receiverArgs),
      testCase "field and indirect-call lowering preserve an indexed UFCS receiver" $ do
        CompUnit resolvedImports resolvedDecls <- resolveSource fieldReceiverSource
        let fieldLowered = fieldDesugarTopDecls resolvedDecls
            fieldReceiverArgs =
              mapMaybe firstArgument (callsNamed receiverMethodName fieldLowered)
        assertBool
          "field lowering did not lower the indexed receiver before the method call"
          (any isLoweredIndexReceiver fieldReceiverArgs)
        (directDecls, _) <- indirectCallTopDecls fieldLowered
        let directMethodCalls = callsNamed receiverMethodName (CompUnit resolvedImports directDecls)
        length directMethodCalls @?= 1,
      testCase "rejects a value member with no matching trait method" $ do
        parsed <- parseSource invalidMemberSource
        resolved <- nameResolution parsed
        case resolved of
          Left err ->
            assertBool
              ("unexpected diagnostic:\n" ++ compilerErrorText err)
              ( "SC0124" `isInfixOf` compilerErrorText err
                  && "value member call: missing" `isInfixOf` compilerErrorText err
              )
          Right unit ->
            assertFailure ("invalid member unexpectedly resolved: " ++ show unit),
      testCase "compiles every value receiver form end to end" $ do
        let folder = "test" </> "examples" </> "ufcs"
            fixture = folder </> "value-receivers.solc"
            options =
              (emptyOption fixture)
                { optRootDir = folder,
                  optNoGenDispatch = True
                }
        result <- compile options
        case result of
          Left err -> assertFailure err
          Right _ -> pure ()
    ]

receiverMethodName :: Name
receiverMethodName = QualName "ReceiverMethod" "project"

firstArgument :: [Exp Name] -> Maybe (Exp Name)
firstArgument [] = Nothing
firstArgument (argument : _) = Just argument

callsNamed :: (Data a) => Name -> a -> [[Exp Name]]
callsNamed expected =
  everything (++) ([] `mkQ` collect)
  where
    collect (Call _ actual arguments)
      | actual == expected = [arguments]
    collect _ = []

isVar :: Name -> Exp Name -> Bool
isVar expected (Var actual) = actual == expected
isVar _ _ = False

isIndexedReceiver :: Exp Name -> Bool
isIndexedReceiver (Indexed (Var values) (Var index)) =
  values == "values" && index == "index"
isIndexedReceiver _ = False

isComputedReceiver :: Exp Name -> Bool
isComputedReceiver (Call Nothing calleeName _) =
  calleeName == QualName "Add" "add"
isComputedReceiver _ = False

isLoweredIndexReceiver :: Exp Name -> Bool
isLoweredIndexReceiver (Call Nothing calleeName _) =
  calleeName == "ridx"
isLoweredIndexReceiver _ = False

parseSource :: String -> IO Surface.CompUnit
parseSource source = do
  parsed <- parseCompUnit source
  case parsed of
    Left err -> assertFailure ("parse failed:\n" ++ err) >> error "unreachable"
    Right unit -> pure unit

resolveSource :: String -> IO (CompUnit Name)
resolveSource source = do
  parsed <- parseSource source
  resolved <- nameResolution parsed
  case resolved of
    Left err ->
      assertFailure ("name resolution failed:\n" ++ compilerErrorText err)
        >> error "unreachable"
    Right unit -> pure unit

valueReceiverSource :: String
valueReceiverSource =
  unlines
    [ "trait ReceiverMethod<a> {",
      "  function project(value: a, salt: word) returns (word);",
      "}",
      "function make(value: word) returns (word) { return value; }",
      "function use(values: word, index: word, param: word) returns (word) {",
      "  let project: word = 99;",
      "  let local: word = param;",
      "  let fromParam: word = param.project(1);",
      "  let fromLocal: word = local.project(2);",
      "  let fromIndex: word = values[index].project(3);",
      "  return (make(param) + 4).project(5);",
      "}"
    ]

fieldReceiverSource :: String
fieldReceiverSource =
  unlines
    [ "trait ReceiverMethod<a> {",
      "  function project(value: a, salt: word) returns (word);",
      "}",
      "contract ReceiverContract {",
      "  values: word;",
      "  function use(index: word) returns (word) {",
      "    return values[index].project(1);",
      "  }",
      "}"
    ]

invalidMemberSource :: String
invalidMemberSource =
  "function bad(value: word) returns (word) { return value.missing(); }"
