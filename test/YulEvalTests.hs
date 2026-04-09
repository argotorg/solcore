module YulEvalTests (yulEvalTests) where

import Data.Map.Strict qualified as Map
import Language.Yul (YLiteral (..), YulExp (..), YulStmt (..))
import Solcore.Backend.MastEval
import Solcore.Frontend.Syntax.Name (Name (..))
import Test.Tasty
import Test.Tasty.HUnit

-- Shorthand constructors for building Yul AST in tests
yIdent :: String -> YulExp
yIdent = YIdent . Name

yNum :: Integer -> YulExp
yNum = YLit . YulNumber

yCall :: String -> [YulExp] -> YulExp
yCall op args = YCall (Name op) args

yAssign :: String -> YulExp -> YulStmt
yAssign n e = YAssign [Name n] e

st :: [(String, Integer)] -> YulState
st = Map.fromList . map (\(n, v) -> (Name n, v))

yulEvalTests :: TestTree
yulEvalTests =
  testGroup
    "Yul arithmetic interpreter"
    [ evalYulOpTests,
      evalYulExpTests,
      evalYulBlockTests,
      asmIsInterpretableTests
    ]

-----------------------------------------------------------------------
-- evalYulOp
-----------------------------------------------------------------------

evalYulOpTests :: TestTree
evalYulOpTests =
  testGroup
    "evalYulOp"
    [ testCase "add: 3 + 5 = 8" $
        evalYulOp (Name "add") [3, 5] @?= Just 8,
      testCase "add: 0 + 0 = 0" $
        evalYulOp (Name "add") [0, 0] @?= Just 0,
      testCase "add: identity with 0" $
        evalYulOp (Name "add") [42, 0] @?= Just 42,
      testCase "add: wraps at 2^256" $
        evalYulOp (Name "add") [maskWord (-1), 1] @?= Just 0,
      testCase "add: large numbers stay within 256 bits" $
        evalYulOp (Name "add") [maskWord (-1), maskWord (-1)]
          @?= Just (maskWord (-2)),
      testCase "mul: 3 * 5 = 15" $
        evalYulOp (Name "mul") [3, 5] @?= Just 15,
      testCase "mul: 0 * anything = 0" $
        evalYulOp (Name "mul") [0, 99] @?= Just 0,
      testCase "mul: 1 * anything = anything" $
        evalYulOp (Name "mul") [1, 7] @?= Just 7,
      testCase "mul: wraps at 2^256" $
        evalYulOp (Name "mul") [2 ^ (128 :: Integer), 2 ^ (128 :: Integer)] @?= Just 0,
      testCase "sload: unsupported → Nothing" $
        evalYulOp (Name "sload") [0] @?= Nothing,
      testCase "mload: unsupported → Nothing" $
        evalYulOp (Name "mload") [0] @?= Nothing,
      testCase "sub: unsupported (not yet in interpreter) → Nothing" $
        evalYulOp (Name "sub") [10, 3] @?= Nothing,
      testCase "add with wrong arity → Nothing" $
        evalYulOp (Name "add") [1, 2, 3] @?= Nothing,
      testCase "unknown op → Nothing" $
        evalYulOp (Name "frobnicate") [1, 2] @?= Nothing
    ]

-----------------------------------------------------------------------
-- evalYulExp
-----------------------------------------------------------------------

evalYulExpTests :: TestTree
evalYulExpTests =
  testGroup
    "evalYulExp"
    [ testCase "YLit number → its value" $
        evalYulExp Map.empty (yNum 42) @?= Just 42,
      testCase "YLit true → 1" $
        evalYulExp Map.empty (YLit YulTrue) @?= Just 1,
      testCase "YLit false → 0" $
        evalYulExp Map.empty (YLit YulFalse) @?= Just 0,
      testCase "YIdent: known variable → its value" $
        evalYulExp (st [("x", 5)]) (yIdent "x") @?= Just 5,
      testCase "YIdent: unknown variable → Nothing" $
        evalYulExp Map.empty (yIdent "x") @?= Nothing,
      testCase "YCall add with two literals" $
        evalYulExp Map.empty (yCall "add" [yNum 3, yNum 5]) @?= Just 8,
      testCase "YCall add with variable and literal" $
        evalYulExp (st [("x", 3)]) (yCall "add" [yIdent "x", yNum 5]) @?= Just 8,
      testCase "YCall add with two variables" $
        evalYulExp (st [("x", 4), ("y", 6)]) (yCall "add" [yIdent "x", yIdent "y"])
          @?= Just 10,
      testCase "YCall add: one unknown variable → Nothing" $
        evalYulExp (st [("x", 4)]) (yCall "add" [yIdent "x", yIdent "y"])
          @?= Nothing,
      testCase "YCall mul with literals" $
        evalYulExp Map.empty (yCall "mul" [yNum 6, yNum 7]) @?= Just 42,
      testCase "YCall nested: mul(add(2,3), 4)" $
        evalYulExp Map.empty (yCall "mul" [yCall "add" [yNum 2, yNum 3], yNum 4])
          @?= Just 20,
      testCase "YCall sload: unsupported → Nothing" $
        evalYulExp Map.empty (yCall "sload" [yNum 0]) @?= Nothing,
      testCase "YCall with unknown arg makes whole call Nothing" $
        evalYulExp Map.empty (yCall "add" [yIdent "unknown", yNum 1]) @?= Nothing
    ]

-----------------------------------------------------------------------
-- evalYulBlock
-----------------------------------------------------------------------

evalYulBlockTests :: TestTree
evalYulBlockTests =
  testGroup
    "evalYulBlock"
    [ testCase "empty block leaves state unchanged" $
        evalYulBlock (st [("x", 5)]) [] @?= Just (st [("x", 5)]),
      testCase "single assign: rw := add(x, y)" $
        evalYulBlock
          (st [("x", 3), ("y", 5)])
          [yAssign "rw" (yCall "add" [yIdent "x", yIdent "y"])]
          @?= Just (st [("rw", 8), ("x", 3), ("y", 5)]),
      testCase "single assign: rw := mul(x, y)" $
        evalYulBlock
          (st [("x", 3), ("y", 5)])
          [yAssign "rw" (yCall "mul" [yIdent "x", yIdent "y"])]
          @?= Just (st [("rw", 15), ("x", 3), ("y", 5)]),
      testCase "assign from literal: rw := 42" $
        evalYulBlock
          Map.empty
          [yAssign "rw" (yNum 42)]
          @?= Just (st [("rw", 42)]),
      testCase "chain of assigns: a := add(x,y); b := mul(a,x)" $
        evalYulBlock
          (st [("x", 3), ("y", 5)])
          [ yAssign "a" (yCall "add" [yIdent "x", yIdent "y"]),
            yAssign "b" (yCall "mul" [yIdent "a", yIdent "x"])
          ]
          @?= Just (st [("a", 8), ("b", 24), ("x", 3), ("y", 5)]),
      testCase "reassigning a variable updates it in state" $
        evalYulBlock
          (st [("x", 1)])
          [ yAssign "x" (yCall "add" [yIdent "x", yNum 1]),
            yAssign "x" (yCall "mul" [yIdent "x", yNum 3])
          ]
          @?= Just (st [("x", 6)]),
      testCase "unsupported op → Nothing for whole block" $
        evalYulBlock
          (st [("x", 1)])
          [yAssign "rw" (yCall "sload" [yNum 0])]
          @?= Nothing,
      testCase "unsupported op in second stmt → Nothing" $
        evalYulBlock
          (st [("x", 3), ("y", 5)])
          [ yAssign "a" (yCall "add" [yIdent "x", yIdent "y"]),
            yAssign "b" (yCall "sload" [yNum 0])
          ]
          @?= Nothing,
      testCase "unknown variable propagates to Nothing" $
        evalYulBlock
          Map.empty
          [yAssign "rw" (yCall "add" [yIdent "x", yNum 1])]
          @?= Nothing
    ]

-----------------------------------------------------------------------
-- asmIsInterpretable
-----------------------------------------------------------------------

asmIsInterpretableTests :: TestTree
asmIsInterpretableTests =
  testGroup
    "asmIsInterpretable"
    [ testCase "empty block → True" $
        asmIsInterpretable [] @?= True,
      testCase "add-assign → True" $
        asmIsInterpretable [yAssign "rw" (yCall "add" [yIdent "x", yIdent "y"])]
          @?= True,
      testCase "mul-assign → True" $
        asmIsInterpretable [yAssign "rw" (yCall "mul" [yIdent "x", yIdent "y"])]
          @?= True,
      testCase "literal-assign → True" $
        asmIsInterpretable [yAssign "rw" (yNum 42)] @?= True,
      testCase "nested arithmetic → True" $
        asmIsInterpretable
          [yAssign "rw" (yCall "mul" [yCall "add" [yIdent "x", yNum 1], yIdent "y"])]
          @?= True,
      testCase "sload-assign → False" $
        asmIsInterpretable [yAssign "rw" (yCall "sload" [yNum 0])]
          @?= False,
      testCase "mload-assign → False" $
        asmIsInterpretable [yAssign "rw" (yCall "mload" [yNum 0])]
          @?= False,
      testCase "sub-assign → False (not yet in interpreter)" $
        asmIsInterpretable [yAssign "rw" (yCall "sub" [yIdent "x", yIdent "y"])]
          @?= False,
      testCase "mstore expression stmt → False (not a single-assign)" $
        asmIsInterpretable [YExp (yCall "mstore" [yNum 0, yIdent "v"])]
          @?= False,
      testCase "multi-assign form → False" $
        asmIsInterpretable [YAssign [Name "a", Name "b"] (yNum 0)]
          @?= False,
      testCase "mix: first stmt ok, second sload → False" $
        asmIsInterpretable
          [ yAssign "a" (yCall "add" [yIdent "x", yIdent "y"]),
            yAssign "b" (yCall "sload" [yNum 0])
          ]
          @?= False
    ]
