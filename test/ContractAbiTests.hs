module ContractAbiTests where

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
        contractAbiJson richContract @?= richExpected
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
