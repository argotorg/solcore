module KeccakTests where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Numeric (showHex)
import Solcore.Util.Keccak (keccak256)
import Test.Tasty
import Test.Tasty.HUnit

hexOf :: BS.ByteString -> String
hexOf = concatMap byte . BS.unpack
  where
    byte b = let h = showHex b "" in if length h == 1 then '0' : h else h

-- Canonical, independently-published Keccak-256 (Ethereum variant) vectors.
-- These cover the single-block absorb path, which is the only one the compiler
-- exercises (keccakLit hashes short function-signature strings).
keccakTests :: TestTree
keccakTests =
  testGroup
    "Keccak-256"
    [ testCase "empty string" $
        hexOf (keccak256 (C8.pack "")) @?= "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470",
      testCase "abc" $
        hexOf (keccak256 (C8.pack "abc")) @?= "4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45",
      testCase "pangram" $
        hexOf (keccak256 (C8.pack "The quick brown fox jumps over the lazy dog"))
          @?= "4d741b6f1eb29cb2a9b9911c82f56fa8d73b04959d3d9d222895df6c0b28aa15"
    ]
