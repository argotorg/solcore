module ContractAbiTests where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.List (isInfixOf)
import Solcore.Desugarer.ContractDispatch (contractAbiJson)
import Solcore.Diagnostics (compilerErrorText)
import Solcore.Frontend.Parser.SolcoreParser (parseCompUnit)
import Solcore.Frontend.Syntax
import Solcore.Frontend.Syntax.NameResolution (nameResolution)
import Solcore.Primitives.Primitives (word)
import Test.Tasty
import Test.Tasty.HUnit

contractAbiTests :: TestTree
contractAbiTests =
  testGroup
    "Contract ABI generation"
    [ testCase "only public functions are exposed" $
        contractAbiJson onlyPublicContract @?= onlyPublicExpected,
      testCase "constructor, payable, word and tuple returns" $
        contractAbiJson richContract @?= richExpected,
      testCase "one named tuple return stays one ABI tuple output" $ do
        contractDef <-
          resolvedContractFromSource
            "contract Reader { function read() external returns (result: (word, bool)) { result = (1, true); return; } }"
        let abi = contractAbiJson contractDef
        assertBool "tuple output keeps its name" ("\"name\": \"result\"" `isInfixOf` abi)
        assertBool "tuple output is not flattened" ("\"type\": \"tuple\"" `isInfixOf` abi),
      testCase "two scalar returns stay two ABI outputs" $ do
        contractDef <-
          resolvedContractFromSource
            "contract Reader { function read() external returns (left: word, right: bool) { left = 1; right = true; return; } }"
        let abi = contractAbiJson contractDef
        assertBool "first scalar name survives" ("\"name\": \"left\"" `isInfixOf` abi)
        assertBool "second scalar name survives" ("\"name\": \"right\"" `isInfixOf` abi)
        assertBool "scalar outputs are not wrapped in a tuple ABI item" (not ("\"type\": \"tuple\"" `isInfixOf` abi)),
      testCase "contract ABI preserves all four function mutability values" $ do
        contractDef <-
          resolvedContractFromSource $
            unlines
              [ "contract Modes {",
                "  function compute() external pure returns (word) { return 0; }",
                "  function inspect() public view returns (word) { return 0; }",
                "  function deposit() external payable { return; }",
                "  function update() public { return; }",
                "}"
              ]
        let abi = contractAbiJson contractDef
        assertAbiFunctionMutability "compute" "pure" abi
        assertAbiFunctionMutability "inspect" "view" abi
        assertAbiFunctionMutability "deposit" "payable" abi
        assertAbiFunctionMutability "update" "nonpayable" abi,
      testCase "interface and library ABI preserve source mutability" $ do
        interfaceDef <-
          resolvedContractFromSource
            "interface Reader { function read() external view returns (word); }"
        libraryDef <-
          resolvedContractFromSource
            "library Math { function twice(x: word) public pure returns (word) { return x + x; } }"
        assertAbiFunctionMutability "read" "view" (contractAbiJson interfaceDef)
        assertAbiFunctionMutability "twice" "pure" (contractAbiJson libraryDef),
      testCase "exact visibility controls ABI exposure when legacy flags disagree" $ do
        let externalSig =
              (sig "externalFn" [] (Just word) False)
                { sigModifiers = [VisibilityModifier VisibilityExternal]
                }
            privateSig =
              (sig "privateFn" [] (Just word) False)
                { sigModifiers = [VisibilityModifier VisibilityPrivate]
                }
            abi =
              contractAbiJson $
                Contract
                  (Name "Visibility")
                  []
                  [ fun False externalSig,
                    fun True privateSig
                  ]
        assertBool
          "external metadata exposes a function even if the legacy flag is false"
          ("\"name\": \"externalFn\"" `isInfixOf` abi)
        assertBool
          "private metadata hides a function even if the legacy flag is true"
          (not ("\"name\": \"privateFn\"" `isInfixOf` abi)),
      testCase "parameterized parameter type fails loudly" $ do
        -- A public function whose parameter is a parameterized type
        -- (e.g. `mapping(word, word)`) has no ABI spelling. Dropping the type
        -- arguments would emit a bare, invalid `"type":"mapping"` string, so the
        -- emitter must fail loudly instead.
        result <- try (evaluate (length (contractAbiJson mappingParamContract)))
        case result of
          Left (ErrorCall msg) ->
            assertBool
              ("unexpected error message: " <> msg)
              ("cannot represent type in ABI" `isInfixOf` msg)
          Right _ ->
            assertFailure "expected ABI emission to fail for a parameterized parameter type"
    ]

resolvedContractFromSource :: String -> IO (Contract Name)
resolvedContractFromSource source = do
  parsedResult <- parseCompUnit source
  parsed <-
    case parsedResult of
      Left err -> assertFailure ("unexpected parse failure:\n" <> err)
      Right compUnit -> pure compUnit
  resolvedResult <- nameResolution parsed
  case resolvedResult of
    Left err ->
      assertFailure
        ("unexpected name-resolution failure:\n" <> compilerErrorText err)
    Right (CompUnit _ topDecls) ->
      case [contractDef | TContr contractDef <- topDecls] of
        [contractDef] -> pure contractDef
        _ -> assertFailure ("unexpected resolved shape: " <> show topDecls)

assertAbiFunctionMutability :: String -> String -> String -> Assertion
assertAbiFunctionMutability functionName expectedMutability abi =
  case dropWhile (not . isInfixOf nameLine) (lines abi) of
    [] ->
      assertFailure
        ("ABI did not contain function " <> show functionName <> ":\n" <> abi)
    functionTail ->
      let functionEntry =
            takeWhile
              (not . isInfixOf "\"type\": \"function\"")
              functionTail
       in assertBool
            ( "ABI function "
                <> show functionName
                <> " did not have stateMutability "
                <> show expectedMutability
                <> ":\n"
                <> unlines functionEntry
            )
            (any (isInfixOf mutabilityLine) functionEntry)
  where
    nameLine = "\"name\": \"" <> functionName <> "\""
    mutabilityLine = "\"stateMutability\": \"" <> expectedMutability <> "\""

-- Helpers for building sample contracts

tyCon :: String -> Ty
tyCon n = TyCon (Name n) []

sig :: String -> [Param Name] -> Maybe Ty -> Bool -> Signature Name
sig fname params ret payable =
  SignatureWithReturnNames
    { sigVars = [],
      sigContext = [],
      sigName = Name fname,
      sigParams = params,
      sigRetComptime = False,
      sigReturn = ret,
      sigPayable = payable,
      sigReturnNames = [],
      sigReturnItems = [],
      sigModifiers = [MutabilityModifier MutabilityPayable | payable]
    }

fun :: Bool -> Signature Name -> ContractDecl Name
fun isPublic s = CFunDecl (FunDef isPublic s [])

-- A contract with one public and one private function.

onlyPublicContract :: Contract Name
onlyPublicContract =
  Contract
    (Name "Sample")
    []
    [ fun True (sig "get" [] (Just (tyCon "uint256")) False),
      fun False (sig "secret" [] (Just (tyCon "uint256")) False)
    ]

onlyPublicExpected :: String
onlyPublicExpected =
  unlines
    [ "[",
      "  {",
      "    \"inputs\": [],",
      "    \"name\": \"get\",",
      "    \"outputs\": [",
      "      {",
      "        \"internalType\": \"uint256\",",
      "        \"name\": \"\",",
      "        \"type\": \"uint256\"",
      "      }",
      "    ],",
      "    \"stateMutability\": \"nonpayable\",",
      "    \"type\": \"function\"",
      "  }",
      "]"
    ]

-- A contract exercising a constructor, a payable function, the native `word`
-- type (mapped to uint256) and a tuple return flattened to two outputs.

richContract :: Contract Name
richContract =
  Contract
    (Name "Token")
    []
    [ CConstrDecl (Constructor [Typed False (Name "amount") word] [] False),
      fun
        True
        ( sig
            "pay"
            [Typed False (Name "to") (tyCon "address")]
            (Just (TyCon (Name "pair") [word, tyCon "bool"]))
            True
        )
    ]

-- A contract with a public function taking a parameterized type that the ABI
-- emitter cannot represent (here `mapping(word, word)`).

mappingParamContract :: Contract Name
mappingParamContract =
  Contract
    (Name "Store")
    []
    [ fun
        True
        ( sig
            "put"
            [Typed False (Name "m") (TyCon (Name "mapping") [word, word])]
            (Just (tyCon "uint256"))
            False
        )
    ]

richExpected :: String
richExpected =
  unlines
    [ "[",
      "  {",
      "    \"inputs\": [",
      "      {",
      "        \"internalType\": \"uint256\",",
      "        \"name\": \"amount\",",
      "        \"type\": \"uint256\"",
      "      }",
      "    ],",
      "    \"stateMutability\": \"nonpayable\",",
      "    \"type\": \"constructor\"",
      "  },",
      "  {",
      "    \"inputs\": [",
      "      {",
      "        \"internalType\": \"address\",",
      "        \"name\": \"to\",",
      "        \"type\": \"address\"",
      "      }",
      "    ],",
      "    \"name\": \"pay\",",
      "    \"outputs\": [",
      "      {",
      "        \"internalType\": \"uint256\",",
      "        \"name\": \"\",",
      "        \"type\": \"uint256\"",
      "      },",
      "      {",
      "        \"internalType\": \"bool\",",
      "        \"name\": \"\",",
      "        \"type\": \"bool\"",
      "      }",
      "    ],",
      "    \"stateMutability\": \"payable\",",
      "    \"type\": \"function\"",
      "  }",
      "]"
    ]
