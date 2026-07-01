module ContractAbiTests where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.List (isInfixOf)
import Solcore.Desugarer.ContractDispatch (contractAbiJson)
import Solcore.Frontend.Syntax
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

-- Helpers for building sample contracts

tyCon :: String -> Ty
tyCon n = TyCon (Name n) []

sig :: String -> [Param Name] -> Maybe Ty -> Bool -> Signature Name
sig fname params ret payable =
  Signature
    { sigVars = [],
      sigContext = [],
      sigName = Name fname,
      sigParams = params,
      sigRetComptime = False,
      sigReturn = ret,
      sigPayable = payable
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
