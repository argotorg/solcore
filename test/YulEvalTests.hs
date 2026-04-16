module YulEvalTests (yulEvalTests) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word8)
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

yExp :: String -> [YulExp] -> YulStmt
yExp op args = YExp (YCall (Name op) args)

st :: [(String, Integer)] -> YulState
st = Map.fromList . map (\(n, v) -> (Name n, v))

-- Run an EvalM action in non-comptime mode (memory ops inactive).
runPure :: EvalM a -> a
runPure m = fst $ runEvalM (EvalEnv Map.empty Set.empty False) defaultFuel m

-- Run an EvalM action in comptime mode (memory ops active).
runComptime :: EvalM a -> a
runComptime m = fst $ runEvalM (EvalEnv Map.empty Set.empty True) defaultFuel m

yulEvalTests :: TestTree
yulEvalTests =
  testGroup
    "Yul interpreter"
    [ evalYulOpTests,
      evalYulExpTests,
      evalYulBlockTests,
      memoryHelperTests,
      memoryEvalTests,
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
        runPure (evalYulOp (Name "add") [3, 5]) @?= Just 8,
      testCase "add: 0 + 0 = 0" $
        runPure (evalYulOp (Name "add") [0, 0]) @?= Just 0,
      testCase "add: identity with 0" $
        runPure (evalYulOp (Name "add") [42, 0]) @?= Just 42,
      testCase "add: wraps at 2^256" $
        runPure (evalYulOp (Name "add") [maskWord (-1), 1]) @?= Just 0,
      testCase "add: large numbers stay within 256 bits" $
        runPure (evalYulOp (Name "add") [maskWord (-1), maskWord (-1)])
          @?= Just (maskWord (-2)),
      testCase "mul: 3 * 5 = 15" $
        runPure (evalYulOp (Name "mul") [3, 5]) @?= Just 15,
      testCase "mul: 0 * anything = 0" $
        runPure (evalYulOp (Name "mul") [0, 99]) @?= Just 0,
      testCase "mul: 1 * anything = anything" $
        runPure (evalYulOp (Name "mul") [1, 7]) @?= Just 7,
      testCase "mul: wraps at 2^256" $
        runPure (evalYulOp (Name "mul") [2 ^ (128 :: Integer), 2 ^ (128 :: Integer)]) @?= Just 0,
      testCase "sload: unsupported → Nothing" $
        runPure (evalYulOp (Name "sload") [0]) @?= Nothing,
      testCase "sub: unsupported (not yet in interpreter) → Nothing" $
        runPure (evalYulOp (Name "sub") [10, 3]) @?= Nothing,
      testCase "add with wrong arity → Nothing" $
        runPure (evalYulOp (Name "add") [1, 2, 3]) @?= Nothing,
      testCase "unknown op → Nothing" $
        runPure (evalYulOp (Name "frobnicate") [1, 2]) @?= Nothing
    ]

-----------------------------------------------------------------------
-- evalYulExp
-----------------------------------------------------------------------

evalYulExpTests :: TestTree
evalYulExpTests =
  testGroup
    "evalYulExp"
    [ testCase "YLit number → its value" $
        runPure (evalYulExp Map.empty (yNum 42)) @?= Just 42,
      testCase "YLit true → 1" $
        runPure (evalYulExp Map.empty (YLit YulTrue)) @?= Just 1,
      testCase "YLit false → 0" $
        runPure (evalYulExp Map.empty (YLit YulFalse)) @?= Just 0,
      testCase "YIdent: known variable → its value" $
        runPure (evalYulExp (st [("x", 5)]) (yIdent "x")) @?= Just 5,
      testCase "YIdent: unknown variable → Nothing" $
        runPure (evalYulExp Map.empty (yIdent "x")) @?= Nothing,
      testCase "YCall add with two literals" $
        runPure (evalYulExp Map.empty (yCall "add" [yNum 3, yNum 5])) @?= Just 8,
      testCase "YCall add with variable and literal" $
        runPure (evalYulExp (st [("x", 3)]) (yCall "add" [yIdent "x", yNum 5])) @?= Just 8,
      testCase "YCall add with two variables" $
        runPure (evalYulExp (st [("x", 4), ("y", 6)]) (yCall "add" [yIdent "x", yIdent "y"]))
          @?= Just 10,
      testCase "YCall add: one unknown variable → Nothing" $
        runPure (evalYulExp (st [("x", 4)]) (yCall "add" [yIdent "x", yIdent "y"]))
          @?= Nothing,
      testCase "YCall mul with literals" $
        runPure (evalYulExp Map.empty (yCall "mul" [yNum 6, yNum 7])) @?= Just 42,
      testCase "YCall nested: mul(add(2,3), 4)" $
        runPure (evalYulExp Map.empty (yCall "mul" [yCall "add" [yNum 2, yNum 3], yNum 4]))
          @?= Just 20,
      testCase "YCall sload: unsupported → Nothing" $
        runPure (evalYulExp Map.empty (yCall "sload" [yNum 0])) @?= Nothing,
      testCase "YCall with unknown arg makes whole call Nothing" $
        runPure (evalYulExp Map.empty (yCall "add" [yIdent "unknown", yNum 1])) @?= Nothing
    ]

-----------------------------------------------------------------------
-- evalYulBlock
-----------------------------------------------------------------------

evalYulBlockTests :: TestTree
evalYulBlockTests =
  testGroup
    "evalYulBlock"
    [ testCase "empty block leaves state unchanged" $
        runPure (evalYulBlock (st [("x", 5)]) []) @?= Just (st [("x", 5)]),
      testCase "single assign: rw := add(x, y)" $
        runPure
          ( evalYulBlock
              (st [("x", 3), ("y", 5)])
              [yAssign "rw" (yCall "add" [yIdent "x", yIdent "y"])]
          )
          @?= Just (st [("rw", 8), ("x", 3), ("y", 5)]),
      testCase "single assign: rw := mul(x, y)" $
        runPure
          ( evalYulBlock
              (st [("x", 3), ("y", 5)])
              [yAssign "rw" (yCall "mul" [yIdent "x", yIdent "y"])]
          )
          @?= Just (st [("rw", 15), ("x", 3), ("y", 5)]),
      testCase "assign from literal: rw := 42" $
        runPure (evalYulBlock Map.empty [yAssign "rw" (yNum 42)])
          @?= Just (st [("rw", 42)]),
      testCase "chain of assigns: a := add(x,y); b := mul(a,x)" $
        runPure
          ( evalYulBlock
              (st [("x", 3), ("y", 5)])
              [ yAssign "a" (yCall "add" [yIdent "x", yIdent "y"]),
                yAssign "b" (yCall "mul" [yIdent "a", yIdent "x"])
              ]
          )
          @?= Just (st [("a", 8), ("b", 24), ("x", 3), ("y", 5)]),
      testCase "reassigning a variable updates it in state" $
        runPure
          ( evalYulBlock
              (st [("x", 1)])
              [ yAssign "x" (yCall "add" [yIdent "x", yNum 1]),
                yAssign "x" (yCall "mul" [yIdent "x", yNum 3])
              ]
          )
          @?= Just (st [("x", 6)]),
      testCase "unsupported op → Nothing for whole block" $
        runPure (evalYulBlock (st [("x", 1)]) [yAssign "rw" (yCall "sload" [yNum 0])])
          @?= Nothing,
      testCase "unsupported op in second stmt → Nothing" $
        runPure
          ( evalYulBlock
              (st [("x", 3), ("y", 5)])
              [ yAssign "a" (yCall "add" [yIdent "x", yIdent "y"]),
                yAssign "b" (yCall "sload" [yNum 0])
              ]
          )
          @?= Nothing,
      testCase "unknown variable propagates to Nothing" $
        runPure (evalYulBlock Map.empty [yAssign "rw" (yCall "add" [yIdent "x", yNum 1])])
          @?= Nothing
    ]

-----------------------------------------------------------------------
-- Memory helpers (pure functions: mstoreBytes, mloadWord)
-----------------------------------------------------------------------

memoryHelperTests :: TestTree
memoryHelperTests =
  testGroup
    "Memory helpers (mstoreBytes / mloadWord)"
    [ testCase "round-trip: store then load recovers value" $
        mloadWord 0 (mstoreBytes 0 42 Map.empty) @?= Just 42,
      testCase "round-trip: non-zero address" $
        mloadWord 64 (mstoreBytes 64 999 Map.empty) @?= Just 999,
      testCase "round-trip: max word value" $
        mloadWord 0 (mstoreBytes 0 (maskWord (-1)) Map.empty) @?= Just (maskWord (-1)),
      testCase "load from unwritten address returns Nothing" $
        -- Cannot assume unwritten bytes are 0: runtime code may have written to memory
        mloadWord 0 Map.empty @?= Nothing,
      testCase "overlapping stores: all 32 bytes covered, value is computable" $ do
        -- mstore(0, x) writes bytes 0..31; mstore(1, y) writes bytes 1..32.
        -- mload(0) reads bytes 0..31: all present (byte 0 from first, 1..31 from second).
        let x = 0x0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20 :: Integer
            y = 0xaabbccdd00000000000000000000000000000000000000000000000000000001 :: Integer
            mem = mstoreBytes 1 y (mstoreBytes 0 x Map.empty)
            result = mloadWord 0 mem
            -- byte 0: from mstore(0,x) → 0x01; bytes 1..31: from mstore(1,y) → 0xaa,0xbb,...
            expected = 0x01aabbccdd000000000000000000000000000000000000000000000000000000
        result @?= Just expected,
      testCase "partial write (mstore8 only): mloadWord returns Nothing" $ do
        -- Only one byte written; the other 31 are unknown → Nothing
        let mem = Map.insert 31 (0x34 :: Word8) Map.empty
        mloadWord 0 mem @?= Nothing
    ]

-----------------------------------------------------------------------
-- Memory operations in Yul evaluator (mstore, mload, mstore8)
-----------------------------------------------------------------------

memoryEvalTests :: TestTree
memoryEvalTests =
  testGroup
    "Memory in Yul evaluator"
    [ testCase "mstore then mload at same address" $
        runComptime
          ( evalYulBlock
              Map.empty
              [ yExp "mstore" [yNum 0, yNum 42],
                yAssign "r" (yCall "mload" [yNum 0])
              ]
          )
          @?= Just (st [("r", 42)]),
      testCase "mstore at non-zero address" $
        runComptime
          ( evalYulBlock
              Map.empty
              [ yExp "mstore" [yNum 32, yNum 100],
                yAssign "r" (yCall "mload" [yNum 32])
              ]
          )
          @?= Just (st [("r", 100)]),
      testCase "mload from unwritten memory returns Nothing" $
        -- Cannot determine mload result without knowing what runtime code wrote there
        runComptime (evalYulBlock Map.empty [yAssign "r" (yCall "mload" [yNum 0])])
          @?= Nothing,
      testCase "mstore8 single byte then mload: Nothing (31 bytes still unknown)" $
        -- mstore8 writes only 1 byte; the other 31 are not in esMem → mload fails
        runComptime
          ( evalYulBlock
              Map.empty
              [ yExp "mstore8" [yNum 31, yNum 0xff],
                yAssign "r" (yCall "mload" [yNum 0])
              ]
          )
          @?= Nothing,
      testCase "mstore8 all 32 bytes, read back via mload" $ do
        -- Write each byte of a 32-byte word individually via mstore8, then mload
        let stmts =
              [yExp "mstore8" [yNum (fromIntegral i), yNum (fromIntegral i + 1)] | i <- [0 .. 31 :: Int]]
                ++ [yAssign "r" (yCall "mload" [yNum 0])]
            -- byte i = i+1, so value = 0x0102...20
            expected = foldl (\acc b -> acc * 256 + b) 0 [1 .. 32]
        runComptime (evalYulBlock Map.empty stmts) @?= Just (st [("r", expected)]),
      testCase "mstore with unknown address → Nothing" $
        runComptime
          ( evalYulBlock
              Map.empty
              [yExp "mstore" [yIdent "unknown_addr", yNum 42]]
          )
          @?= Nothing,
      testCase "mstore with unknown value → Nothing" $
        runComptime
          ( evalYulBlock
              Map.empty
              [yExp "mstore" [yNum 0, yIdent "unknown_val"]]
          )
          @?= Nothing,
      testCase "mload with unknown address → Nothing" $
        runComptime
          ( evalYulBlock
              Map.empty
              [yAssign "r" (yCall "mload" [yIdent "unknown_addr"])]
          )
          @?= Nothing,
      testCase "mstore value from variable" $
        runComptime
          ( evalYulBlock
              (st [("v", 77)])
              [ yExp "mstore" [yNum 0, yIdent "v"],
                yAssign "r" (yCall "mload" [yNum 0])
              ]
          )
          @?= Just (st [("r", 77), ("v", 77)]),
      testCase "chain: two stores, read both back" $
        runComptime
          ( evalYulBlock
              Map.empty
              [ yExp "mstore" [yNum 0, yNum 1],
                yExp "mstore" [yNum 32, yNum 2],
                yAssign "a" (yCall "mload" [yNum 0]),
                yAssign "b" (yCall "mload" [yNum 32])
              ]
          )
          @?= Just (st [("a", 1), ("b", 2)]),
      testCase "mstore in non-comptime mode → Nothing (block aborted)" $
        -- Outside a comptime let, mstore fails the block; prevents unsound inlining
        runPure
          ( evalYulBlock
              Map.empty
              [yExp "mstore" [yNum 0, yNum 42]]
          )
          @?= Nothing,
      testCase "mload in non-comptime mode → Nothing" $
        runPure
          ( evalYulBlock
              Map.empty
              [yAssign "r" (yCall "mload" [yNum 0])]
          )
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
      testCase "mstore expression stmt → True" $
        asmIsInterpretable [yExp "mstore" [yNum 0, yIdent "v"]]
          @?= True,
      testCase "mstore8 expression stmt → True" $
        asmIsInterpretable [yExp "mstore8" [yNum 31, yNum 0xff]]
          @?= True,
      testCase "mload in assignment → True" $
        asmIsInterpretable [yAssign "r" (yCall "mload" [yNum 0])]
          @?= True,
      testCase "sload-assign → False" $
        asmIsInterpretable [yAssign "rw" (yCall "sload" [yNum 0])]
          @?= False,
      testCase "sub-assign → False (not yet in interpreter)" $
        asmIsInterpretable [yAssign "rw" (yCall "sub" [yIdent "x", yIdent "y"])]
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
