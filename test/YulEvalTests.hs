module YulEvalTests (yulEvalTests) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word8)
import Language.Yul (YLiteral (..), YulExp (..), YulStmt (..))
import Solcore.Backend.Mast
import Solcore.Backend.MastEval
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt (Literal (..))
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

-- A Yul state of concrete words
st :: [(String, Integer)] -> YulState
st = Map.fromList . map (\(n, v) -> (Name n, Concrete v))

-- An absolute comptime memory address
at :: Integer -> MemAddr
at n = (AbsoluteMem, n)

-- A comptime memory address at an offset above the free memory pointer
above :: Integer -> MemAddr
above k = (FreeMem, k)

-- Apply a Yul built-in to concrete words, keeping only a concrete result
wordOp :: String -> [Integer] -> Maybe Integer
wordOp op args = runPure (evalYulOp (Name op) (map Concrete args)) >>= asWord

asWord :: YulVal -> Maybe Integer
asWord (Concrete n) = Just n
asWord (FreeOffset _) = Nothing

-- Run an EvalM action in non-comptime mode (memory ops inactive).
runPure :: EvalM a -> a
runPure m = fst $ runEvalM (EvalEnv Map.empty Set.empty False) defaultFuel m

-- Run an EvalM action in comptime mode (memory ops active).
runComptime :: EvalM a -> a
runComptime m = fst $ runEvalM (EvalEnv Map.empty Set.empty True) defaultFuel m

-- Extract the boolean value from a comptime bool (inr = true, inl = false).
asMastBool :: MastExp -> Maybe Bool
asMastBool (MastCon i _)
  | mastIdName i == Name "inr" = Just True
  | mastIdName i == Name "inl" = Just False
asMastBool _ = Nothing

intLit :: Integer -> MastExp
intLit n = MastLit (IntLit n)

yulEvalTests :: TestTree
yulEvalTests =
  testGroup
    "Yul interpreter"
    [ evalPrimitiveTests,
      evalYulOpTests,
      evalYulExpTests,
      evalYulBlockTests,
      memoryHelperTests,
      memoryEvalTests,
      freeMemoryTests,
      keccakEvalTests,
      comptimeFoldingTests,
      asmIsInterpretableTests
    ]

-----------------------------------------------------------------------
-- evalPrimitive: integer builtins
-----------------------------------------------------------------------

evalPrimitiveTests :: TestTree
evalPrimitiveTests =
  testGroup
    "evalPrimitive (integer builtins)"
    [ testCase "wordToInteger: identity" $
        evalPrimitive (Name "wordToInteger") [intLit 42] @?= Just (intLit 42),
      testCase "wordToInteger: max word" $
        evalPrimitive (Name "wordToInteger") [intLit (maskWord (-1))]
          @?= Just (intLit (maskWord (-1))),
      testCase "wordFromInteger: identity below 2^256" $
        evalPrimitive (Name "wordFromInteger") [intLit 42] @?= Just (intLit 42),
      testCase "wordFromInteger: truncates at 2^256" $
        evalPrimitive (Name "wordFromInteger") [intLit (2 ^ (256 :: Integer))]
          @?= Just (intLit 0),
      testCase "wordFromInteger: 2^256 + 1 truncates to 1" $
        evalPrimitive (Name "wordFromInteger") [intLit (2 ^ (256 :: Integer) + 1)]
          @?= Just (intLit 1),
      testCase "integerAdd: no overflow (2^255 + 2^255 = 2^256, not 0)" $
        evalPrimitive (Name "integerAdd") [intLit (2 ^ (255 :: Integer)), intLit (2 ^ (255 :: Integer))]
          @?= Just (intLit (2 ^ (256 :: Integer))),
      testCase "integerAdd: basic" $
        evalPrimitive (Name "integerAdd") [intLit 3, intLit 5] @?= Just (intLit 8),
      testCase "integerSub: basic" $
        evalPrimitive (Name "integerSub") [intLit 10, intLit 3] @?= Just (intLit 7),
      testCase "integerSub: exact (no wrapping)" $
        evalPrimitive (Name "integerSub") [intLit 1, intLit 2] @?= Just (intLit (-1)),
      testCase "integerMul: no overflow (2^128 * 2^128 = 2^256)" $
        evalPrimitive (Name "integerMul") [intLit (2 ^ (128 :: Integer)), intLit (2 ^ (128 :: Integer))]
          @?= Just (intLit (2 ^ (256 :: Integer))),
      testCase "integerLt: true when a < b" $
        (asMastBool =<< evalPrimitive (Name "integerLt") [intLit (2 ^ (256 :: Integer)), intLit (2 ^ (256 :: Integer) + 1)])
          @?= Just True,
      testCase "integerLt: false when a == b" $
        (asMastBool =<< evalPrimitive (Name "integerLt") [intLit 5, intLit 5])
          @?= Just False,
      testCase "integerLt: false when a > b" $
        (asMastBool =<< evalPrimitive (Name "integerLt") [intLit 6, intLit 5])
          @?= Just False,
      testCase "integerEq: true when equal" $
        (asMastBool =<< evalPrimitive (Name "integerEq") [intLit (2 ^ (256 :: Integer)), intLit (2 ^ (256 :: Integer))])
          @?= Just True,
      testCase "integerEq: false when unequal" $
        (asMastBool =<< evalPrimitive (Name "integerEq") [intLit 3, intLit 4])
          @?= Just False,
      testCase "evalPrimitive: wrong arity → Nothing" $
        evalPrimitive (Name "integerAdd") [intLit 1] @?= Nothing,
      testCase "evalPrimitive: non-literal arg → Nothing" $
        let unknownVar = MastVar (MastId (Name "x") (MastTyCon (Name "integer") []))
         in evalPrimitive (Name "integerAdd") [intLit 1, unknownVar] @?= Nothing
    ]

-----------------------------------------------------------------------
-- evalYulOp
-----------------------------------------------------------------------

evalYulOpTests :: TestTree
evalYulOpTests =
  testGroup
    "evalYulOp"
    [ testCase "add: 3 + 5 = 8" $
        wordOp "add" [3, 5] @?= Just 8,
      testCase "add: 0 + 0 = 0" $
        wordOp "add" [0, 0] @?= Just 0,
      testCase "add: identity with 0" $
        wordOp "add" [42, 0] @?= Just 42,
      testCase "add: wraps at 2^256" $
        wordOp "add" [maskWord (-1), 1] @?= Just 0,
      testCase "add: large numbers stay within 256 bits" $
        wordOp "add" [maskWord (-1), maskWord (-1)]
          @?= Just (maskWord (-2)),
      testCase "mul: 3 * 5 = 15" $
        wordOp "mul" [3, 5] @?= Just 15,
      testCase "mul: 0 * anything = 0" $
        wordOp "mul" [0, 99] @?= Just 0,
      testCase "mul: 1 * anything = anything" $
        wordOp "mul" [1, 7] @?= Just 7,
      testCase "mul: wraps at 2^256" $
        wordOp "mul" [2 ^ (128 :: Integer), 2 ^ (128 :: Integer)] @?= Just 0,
      testCase "sload: unsupported → Nothing" $
        wordOp "sload" [0] @?= Nothing,
      testCase "sub: 10 - 3 = 7" $
        wordOp "sub" [10, 3] @?= Just 7,
      testCase "sub: wraps at 2^256" $
        wordOp "sub" [0, 1] @?= Just (2 ^ (256 :: Integer) - 1),
      testCase "gt: 5 > 3 = 1" $
        wordOp "gt" [5, 3] @?= Just 1,
      testCase "gt: 3 > 5 = 0" $
        wordOp "gt" [3, 5] @?= Just 0,
      testCase "lt: 3 < 5 = 1" $
        wordOp "lt" [3, 5] @?= Just 1,
      testCase "eq: 4 == 4 = 1" $
        wordOp "eq" [4, 4] @?= Just 1,
      testCase "eq: 4 == 5 = 0" $
        wordOp "eq" [4, 5] @?= Just 0,
      testCase "iszero: 0 = 1" $
        wordOp "iszero" [0] @?= Just 1,
      testCase "iszero: 1 = 0" $
        wordOp "iszero" [1] @?= Just 0,
      testCase "add with wrong arity → Nothing" $
        wordOp "add" [1, 2, 3] @?= Nothing,
      testCase "unknown op → Nothing" $
        wordOp "frobnicate" [1, 2] @?= Nothing
    ]

-----------------------------------------------------------------------
-- evalYulExp
-----------------------------------------------------------------------

evalYulExpTests :: TestTree
evalYulExpTests =
  testGroup
    "evalYulExp"
    [ testCase "YLit number → its value" $
        runPure (evalYulExp Map.empty (yNum 42)) @?= Just (Concrete 42),
      testCase "YLit true → 1" $
        runPure (evalYulExp Map.empty (YLit YulTrue)) @?= Just (Concrete 1),
      testCase "YLit false → 0" $
        runPure (evalYulExp Map.empty (YLit YulFalse)) @?= Just (Concrete 0),
      testCase "YIdent: known variable → its value" $
        runPure (evalYulExp (st [("x", 5)]) (yIdent "x")) @?= Just (Concrete 5),
      testCase "YIdent: unknown variable → Nothing" $
        runPure (evalYulExp Map.empty (yIdent "x")) @?= Nothing,
      testCase "YCall add with two literals" $
        runPure (evalYulExp Map.empty (yCall "add" [yNum 3, yNum 5])) @?= Just (Concrete 8),
      testCase "YCall add with variable and literal" $
        runPure (evalYulExp (st [("x", 3)]) (yCall "add" [yIdent "x", yNum 5])) @?= Just (Concrete 8),
      testCase "YCall add with two variables" $
        runPure (evalYulExp (st [("x", 4), ("y", 6)]) (yCall "add" [yIdent "x", yIdent "y"]))
          @?= Just (Concrete 10),
      testCase "YCall add: one unknown variable → Nothing" $
        runPure (evalYulExp (st [("x", 4)]) (yCall "add" [yIdent "x", yIdent "y"]))
          @?= Nothing,
      testCase "YCall mul with literals" $
        runPure (evalYulExp Map.empty (yCall "mul" [yNum 6, yNum 7])) @?= Just (Concrete 42),
      testCase "YCall nested: mul(add(2,3), 4)" $
        runPure (evalYulExp Map.empty (yCall "mul" [yCall "add" [yNum 2, yNum 3], yNum 4]))
          @?= Just (Concrete 20),
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
        mloadWord (at 0) (mstoreBytes (at 0) 42 Map.empty) @?= Just 42,
      testCase "round-trip: non-zero address" $
        mloadWord (at 64) (mstoreBytes (at 64) 999 Map.empty) @?= Just 999,
      testCase "round-trip: max word value" $
        mloadWord (at 0) (mstoreBytes (at 0) (maskWord (-1)) Map.empty) @?= Just (maskWord (-1)),
      testCase "load from unwritten address returns Nothing" $
        -- Cannot assume unwritten bytes are 0: runtime code may have written to memory
        mloadWord (at 0) Map.empty @?= Nothing,
      testCase "overlapping stores: all 32 bytes covered, value is computable" $ do
        -- mstore(0, x) writes bytes 0..31; mstore(1, y) writes bytes 1..32.
        -- mload(0) reads bytes 0..31: all present (byte 0 from first, 1..31 from second).
        let x = 0x0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20 :: Integer
            y = 0xaabbccdd00000000000000000000000000000000000000000000000000000001 :: Integer
            mem = mstoreBytes (at 1) y (mstoreBytes (at 0) x Map.empty)
            result = mloadWord (at 0) mem
            -- byte 0: from mstore(0,x) → 0x01; bytes 1..31: from mstore(1,y) → 0xaa,0xbb,...
            expected = 0x01aabbccdd000000000000000000000000000000000000000000000000000000
        result @?= Just expected,
      testCase "partial write (mstore8 only): mloadWord returns Nothing" $ do
        -- Only one byte written; the other 31 are unknown → Nothing
        let mem = Map.insert (at 31) (0x34 :: Word8) Map.empty
        mloadWord (at 0) mem @?= Nothing
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
-- The symbolic free memory pointer
-----------------------------------------------------------------------

freeMemoryTests :: TestTree
freeMemoryTests =
  testGroup
    "Symbolic free memory pointer"
    [ testCase "mload(0x40) yields the symbolic base" $
        runComptime (evalYulExp Map.empty (yCall "mload" [yNum 64]))
          @?= Just (FreeOffset 0),
      testCase "mload(0x40) outside comptime mode → Nothing" $
        runPure (evalYulExp Map.empty (yCall "mload" [yNum 64])) @?= Nothing,
      testCase "add shifts the symbolic offset" $
        runComptime (evalYulOp (Name "add") [FreeOffset 0, Concrete 32])
          @?= Just (FreeOffset 32),
      testCase "add shifts the symbolic offset from either side" $
        runComptime (evalYulOp (Name "add") [Concrete 32, FreeOffset 64])
          @?= Just (FreeOffset 96),
      testCase "sub shifts the symbolic offset" $
        runComptime (evalYulOp (Name "sub") [FreeOffset 64, Concrete 32])
          @?= Just (FreeOffset 32),
      testCase "arithmetic on two symbolic addresses → Nothing" $
        runComptime (evalYulOp (Name "add") [FreeOffset 0, FreeOffset 32]) @?= Nothing,
      testCase "multiplying a symbolic address → Nothing" $
        runComptime (evalYulOp (Name "mul") [FreeOffset 0, Concrete 2]) @?= Nothing,
      testCase "comparing a symbolic address → Nothing" $
        -- Its run-time value is unknown, so no comparison can be decided
        runComptime (evalYulOp (Name "eq") [FreeOffset 0, Concrete 0]) @?= Nothing,
      testCase "scratch space above the free pointer: store then load" $
        runComptime
          ( evalYulBlock
              Map.empty
              [ yAssign "p" (yCall "mload" [yNum 64]),
                yExp "mstore" [yCall "add" [yIdent "p", yNum 32], yNum 7],
                yAssign "r" (yCall "mload" [yCall "add" [yIdent "p", yNum 32]])
              ]
          )
          @?= Just (Map.fromList [(Name "p", FreeOffset 0), (Name "r", Concrete 7)]),
      testCase "scratch space above the free pointer: keccak256 over it" $
        runComptime
          ( evalYulBlock
              Map.empty
              [ yAssign "p" (yCall "mload" [yNum 64]),
                yExp "mstore" [yIdent "p", yNum 42],
                yAssign "h" (yCall "keccak256" [yIdent "p", yNum 32])
              ]
          )
          @?= Just (Map.fromList [(Name "p", FreeOffset 0), (Name "h", Concrete keccak42)]),
      testCase "a symbolic address is never stored as a value" $
        -- Stored, it would be read back as a plain word, losing its unknown base
        runComptime
          ( evalYulBlock
              Map.empty
              [ yAssign "p" (yCall "mload" [yNum 64]),
                yExp "mstore" [yIdent "p", yIdent "p"]
              ]
          )
          @?= Nothing,
      testCase "moving the free memory pointer aborts the evaluation" $
        -- FreeOffset assumes the pointer stays put; reallocation is not modelled
        runComptime (evalYulBlock Map.empty [yExp "mstore" [yNum 64, yNum 0x80]])
          @?= Nothing,
      testCase "a write merely overlapping the pointer slot also aborts" $
        runComptime (evalYulBlock Map.empty [yExp "mstore" [yNum 33, yNum 1]])
          @?= Nothing,
      testCase "mstore8 into the pointer slot aborts too" $
        runComptime (evalYulBlock Map.empty [yExp "mstore8" [yNum 64, yNum 1]])
          @?= Nothing,
      testCase "a write below the pointer slot is unaffected" $
        runComptime
          ( evalYulBlock
              Map.empty
              [ yExp "mstore" [yNum 32, yNum 1],
                yAssign "r" (yCall "mload" [yNum 32])
              ]
          )
          @?= Just (st [("r", 1)]),
      testCase "mixing absolute and free-relative memory → Nothing" $
        -- The two may alias at run time, so one evaluation may use only one region
        runComptime
          ( evalYulBlock
              Map.empty
              [ yExp "mstore" [yNum 0, yNum 1],
                yAssign "p" (yCall "mload" [yNum 64]),
                yExp "mstore" [yIdent "p", yNum 2]
              ]
          )
          @?= Nothing,
      testCase "reading absolute memory after writing free-relative → Nothing" $
        runComptime
          ( evalYulBlock
              Map.empty
              [ yAssign "p" (yCall "mload" [yNum 64]),
                yExp "mstore" [yIdent "p", yNum 1],
                yAssign "r" (yCall "mload" [yNum 0])
              ]
          )
          @?= Nothing,
      testCase "the memory helpers keep the two regions apart" $
        mloadWord (at 0) (mstoreBytes (above 0) 42 Map.empty) @?= Nothing
    ]

-----------------------------------------------------------------------
-- keccak256 over comptime-known memory
-----------------------------------------------------------------------

-- keccak256 of the 32-byte big-endian encoding of 42
keccak42 :: Integer
keccak42 = 0xbeced09521047d05b8960b7e7bcc1d1292cf3e4b2a6b63f48335cbde5f7545d2

-- keccak256 of the empty byte string
keccakEmpty :: Integer
keccakEmpty = 0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470

keccakEvalTests :: TestTree
keccakEvalTests =
  testGroup
    "keccak256 in Yul evaluator"
    [ testCase "mloadRange: reads a written byte range" $
        mloadRange (at 0) 2 (Map.fromList [(at 0, 0xde), (at 1, 0xad)]) @?= Just (BS.pack [0xde, 0xad]),
      testCase "mloadRange: zero length is the empty string" $
        mloadRange (at 100) 0 Map.empty @?= Just BS.empty,
      testCase "mloadRange: any missing byte yields Nothing" $
        mloadRange (at 0) 2 (Map.fromList [(at 0, 0xde)]) @?= Nothing,
      testCase "mstore then keccak256 over the stored word" $
        runComptime
          ( evalYulBlock
              Map.empty
              [ yExp "mstore" [yNum 0, yNum 42],
                yAssign "h" (yCall "keccak256" [yNum 0, yNum 32])
              ]
          )
          @?= Just (st [("h", keccak42)]),
      testCase "keccak256 over a zero-length range needs no memory" $
        runComptime (evalYulBlock Map.empty [yAssign "h" (yCall "keccak256" [yNum 0, yNum 0])])
          @?= Just (st [("h", keccakEmpty)]),
      testCase "keccak256 over unwritten memory → Nothing" $
        runComptime (evalYulBlock Map.empty [yAssign "h" (yCall "keccak256" [yNum 0, yNum 32])])
          @?= Nothing,
      testCase "keccak256 over a partially written range → Nothing" $
        -- 32 bytes written at 0, but the hash covers 64 bytes
        runComptime
          ( evalYulBlock
              Map.empty
              [ yExp "mstore" [yNum 0, yNum 42],
                yAssign "h" (yCall "keccak256" [yNum 0, yNum 64])
              ]
          )
          @?= Nothing,
      testCase "keccak256 with unknown address → Nothing" $
        runComptime
          ( evalYulBlock
              Map.empty
              [yAssign "h" (yCall "keccak256" [yIdent "p", yNum 32])]
          )
          @?= Nothing,
      testCase "keccak256 with unknown length → Nothing" $
        runComptime
          ( evalYulBlock
              Map.empty
              [ yExp "mstore" [yNum 0, yNum 42],
                yAssign "h" (yCall "keccak256" [yNum 0, yIdent "n"])
              ]
          )
          @?= Nothing,
      testCase "keccak256 in non-comptime mode → Nothing" $
        runPure (evalYulBlock Map.empty [yAssign "h" (yCall "keccak256" [yNum 0, yNum 0])])
          @?= Nothing
    ]

-----------------------------------------------------------------------
-- Whole-unit folding of comptime hashing (evalCompUnit)
-----------------------------------------------------------------------

-- These build the MAST that `mstore(0, x); keccak256(0, 32)` specialises to,
-- and check that partial evaluation replaces the call with the hash literal.
-- The .solc-level comptime tests only assert that compilation succeeds, so
-- they cannot tell folding apart from a call left in the output.

wordTy :: MastTy
wordTy = MastTyCon (Name "word") []

unitTy :: MastTy
unitTy = MastTyCon (Name "unit") []

funId :: String -> [MastTy] -> MastTy -> MastId
funId n argTys ret = MastId (Name n) (foldr MastArrow ret argTys)

varId :: String -> MastId
varId n = MastId (Name n) wordTy

param :: String -> MastParam
param n = MastParam (Name n) False wordTy

-- function mstore(a, b) -> () { assembly { mstore(a, b) } }
mstoreDef :: MastFunDef
mstoreDef =
  MastFunDef (Name "mstore") [param "a", param "b"] False unitTy $
    [MastAsm [yExp "mstore" [yIdent "a", yIdent "b"]]]

-- function keccak256(a, b) -> word { let res; assembly { res := keccak256(a, b) } return res; }
keccakDef :: MastFunDef
keccakDef =
  MastFunDef (Name "keccak256") [param "a", param "b"] False wordTy $
    [ MastLet False (varId "res") (Just wordTy) Nothing,
      MastAsm [yAssign "res" (yCall "keccak256" [yIdent "a", yIdent "b"])],
      MastReturn (MastVar (varId "res"))
    ]

-- function hashOne(x) -> <retComptime> word { mstore(0, x); return keccak256(0, 32); }
hashOneDef :: Bool -> MastFunDef
hashOneDef retComptime =
  MastFunDef (Name "hashOne") [param "x"] retComptime wordTy $
    [ MastStmtExp (MastCall (funId "mstore" [wordTy, wordTy] unitTy) [intLit 0, MastVar (varId "x")]),
      MastReturn (MastCall (funId "keccak256" [wordTy, wordTy] wordTy) [intLit 0, intLit 32])
    ]

-- function get_free_memory() -> word { let fp; assembly { fp := mload(0x40) } return fp; }
getFreeMemoryDef :: MastFunDef
getFreeMemoryDef =
  MastFunDef (Name "get_free_memory") [] False wordTy $
    [ MastLet False (varId "fp") (Just wordTy) Nothing,
      MastAsm [yAssign "fp" (yCall "mload" [yNum 64])],
      MastReturn (MastVar (varId "fp"))
    ]

getFreeMemoryCall :: MastExp
getFreeMemoryCall = MastCall (funId "get_free_memory" [] wordTy) []

-- function hashScratch(x) -> comptime word {
--   let p = get_free_memory(); mstore(p, x); return keccak256(p, 32);
-- }
hashScratchDef :: MastFunDef
hashScratchDef =
  MastFunDef (Name "hashScratch") [param "x"] True wordTy $
    [ MastLet False (varId "p") (Just wordTy) (Just getFreeMemoryCall),
      MastStmtExp
        ( MastCall
            (funId "mstore" [wordTy, wordTy] unitTy)
            [MastVar (varId "p"), MastVar (varId "x")]
        ),
      MastReturn
        ( MastCall
            (funId "keccak256" [wordTy, wordTy] wordTy)
            [MastVar (varId "p"), intLit 32]
        )
    ]

-- The body of `main` after partially evaluating a unit made of the given
-- helper definitions plus a `main` with the given body.
foldedMainWith :: [MastFunDef] -> [MastStmt] -> [MastStmt]
foldedMainWith defs mainBody =
  concat [mastFunBody fd | MastCFunDecl fd <- decls', mastFunName fd == Name "main"]
  where
    mainDef = MastFunDef (Name "main") [] False wordTy mainBody
    unit =
      MastCompUnit [] $
        [ MastTContr . MastContract (Name "C") $
            map MastCFunDecl (defs ++ [mainDef])
        ]
    (unit', _) = evalCompUnit defaultFuel unit
    decls' = concat [mastContrDecls c | MastTContr c <- mastTopDecls unit']

-- The body of `main` for a unit whose `hashOne` has the given comptime-return
-- flag and whose `main` has the given body.
foldedMain :: Bool -> [MastStmt] -> [MastStmt]
foldedMain retComptime = foldedMainWith [mstoreDef, keccakDef, hashOneDef retComptime]

comptimeFoldingTests :: TestTree
comptimeFoldingTests =
  testGroup
    "evalCompUnit: comptime hashing"
    [ testCase "comptime let folds a memory hash to its literal" $
        -- let r : comptime word = hashOne(42);  ==>  the let disappears, r is known
        foldedMain
          False
          [ MastLet True (varId "r") (Just wordTy) (Just (MastCall (funId "hashOne" [wordTy] wordTy) [intLit 42])),
            MastReturn (MastVar (varId "r"))
          ]
          @?= [MastReturn (intLit keccak42)],
      testCase "'-> comptime' function folds at its call site without a comptime let" $
        -- return hashOne(42);  where hashOne is annotated '-> comptime word'
        foldedMain True [MastReturn (MastCall (funId "hashOne" [wordTy] wordTy) [intLit 42])]
          @?= [MastReturn (intLit keccak42)],
      testCase "without '-> comptime' and without a comptime let, the call is left alone" $
        -- Memory ops must not run outside comptime context: hashing runtime
        -- memory at compile time would be unsound.
        foldedMain False [MastReturn (MastCall (funId "hashOne" [wordTy] wordTy) [intLit 42])]
          @?= [MastReturn (MastCall (funId "hashOne" [wordTy] wordTy) [intLit 42])],
      testCase "scratch space above the free memory pointer folds" $
        -- The address is unknown, but the offsets from it are enough to lay out
        -- and hash the bytes.
        foldedMainWith
          [mstoreDef, keccakDef, getFreeMemoryDef, hashScratchDef]
          [MastReturn (MastCall (funId "hashScratch" [wordTy] wordTy) [intLit 42])]
          @?= [MastReturn (intLit keccak42)],
      testCase "the free memory pointer itself is not a comptime value" $
        -- Only offsets from the base are tracked; the base has no literal form
        foldedMainWith
          [getFreeMemoryDef]
          [ MastLet True (varId "p") (Just wordTy) (Just getFreeMemoryCall),
            MastReturn (MastVar (varId "p"))
          ]
          @?= [ MastLet True (varId "p") (Just wordTy) (Just getFreeMemoryCall),
                MastReturn (MastVar (varId "p"))
              ]
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
      testCase "keccak256 in assignment → True" $
        asmIsInterpretable [yAssign "r" (yCall "keccak256" [yIdent "a", yIdent "b"])]
          @?= True,
      testCase "sload-assign → False" $
        asmIsInterpretable [yAssign "rw" (yCall "sload" [yNum 0])]
          @?= False,
      testCase "sub-assign → True" $
        asmIsInterpretable [yAssign "rw" (yCall "sub" [yIdent "x", yIdent "y"])]
          @?= True,
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
