{-# LANGUAGE BangPatterns #-}

-- | A pure-Haskell Keccak-256 (the Ethereum variant, i.e. original Keccak
-- padding with the 0x01 domain byte, not FIPS-202 SHA3-256's 0x06).
--
-- This replaces the C-based cryptonite dependency so the compiler can be
-- cross-compiled to JavaScript with the GHC JS backend. It is only used at
-- compile time to evaluate @keccakLit@, so performance is not a concern.
module Solcore.Util.Keccak
  ( keccak256,
  )
where

import Data.Array.Unboxed (UArray, listArray, (!), (//))
import Data.Array.Unboxed qualified as A
import Data.Bits (complement, rotateL, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Word (Word64)

-- | Keccak-256 digest (32 bytes) of a byte string.
keccak256 :: ByteString -> ByteString
keccak256 = squeeze . L.foldl' absorbBlock initialState . chunksOf rateBytes . pad rateBytes

rateBytes :: Int
rateBytes = 136 -- 1088-bit rate for Keccak-256 (17 lanes)

type State = UArray Int Word64

initialState :: State
initialState = listArray (0, 24) (replicate 25 0)

-- Lane index for the 5x5 state, column-major so that idx x y = x + 5*y.
idx :: Int -> Int -> Int
idx x y = x + 5 * y

-- Multi-rate padding pad10*1 with Keccak's 0x01 domain byte.
pad :: Int -> ByteString -> ByteString
pad rate msg = msg <> padding
  where
    q = rate - (BS.length msg `mod` rate)
    padding
      | q == 1 = BS.singleton 0x81
      | otherwise = BS.singleton 0x01 <> BS.replicate (q - 2) 0x00 <> BS.singleton 0x80

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf n bs
  | BS.null bs = []
  | otherwise = BS.take n bs : chunksOf n (BS.drop n bs)

-- Read 8 little-endian bytes at byte offset (8*lane) of a rate block.
laneAt :: ByteString -> Int -> Word64
laneAt block lane =
  L.foldl' (\ !acc k -> acc .|. (fromIntegral (BS.index block (base + k)) `shiftL` (8 * k))) 0 [0 .. 7]
  where
    base = 8 * lane

absorbBlock :: State -> ByteString -> State
absorbBlock st block = keccakF (st // [(i, (st ! i) `xor` laneAt block i) | i <- [0 .. 16]])

squeeze :: State -> ByteString
squeeze st =
  BS.pack
    [fromIntegral ((st ! lane) `shiftR` (8 * byte)) | lane <- [0 .. 3], byte <- [0 .. 7]]

keccakF :: State -> State
keccakF s0 = L.foldl' applyRound s0 roundConstants
  where
    applyRound a rc =
      let cs = listArray (0, 4) [L.foldl' xor 0 [a ! idx x y | y <- [0 .. 4]] | x <- [0 .. 4]] :: UArray Int Word64
          ds = listArray (0, 4) [(cs ! ((x + 4) `mod` 5)) `xor` rotateL (cs ! ((x + 1) `mod` 5)) 1 | x <- [0 .. 4]] :: UArray Int Word64
          theta = A.array (0, 24) [(idx x y, (a ! idx x y) `xor` (ds ! x)) | x <- [0 .. 4], y <- [0 .. 4]] :: UArray Int Word64
          b =
            A.array
              (0, 24)
              [(idx y ((2 * x + 3 * y) `mod` 5), rotateL (theta ! idx x y) (rhoOffsets ! idx x y)) | x <- [0 .. 4], y <- [0 .. 4]] ::
              UArray Int Word64
          chi =
            A.array
              (0, 24)
              [(idx x y, (b ! idx x y) `xor` (complement (b ! idx ((x + 1) `mod` 5) y) .&. (b ! idx ((x + 2) `mod` 5) y))) | x <- [0 .. 4], y <- [0 .. 4]] ::
              UArray Int Word64
       in chi // [(0, (chi ! 0) `xor` rc)]

rhoOffsets :: UArray Int Int
rhoOffsets =
  listArray
    (0, 24)
    [ 0,
      1,
      62,
      28,
      27,
      36,
      44,
      6,
      55,
      20,
      3,
      10,
      43,
      25,
      39,
      41,
      45,
      15,
      21,
      8,
      18,
      2,
      61,
      56,
      14
    ]

roundConstants :: [Word64]
roundConstants =
  [ 0x0000000000000001,
    0x0000000000008082,
    0x800000000000808A,
    0x8000000080008000,
    0x000000000000808B,
    0x0000000080000001,
    0x8000000080008081,
    0x8000000000008009,
    0x000000000000008A,
    0x0000000000000088,
    0x0000000080008009,
    0x000000008000000A,
    0x000000008000808B,
    0x800000000000008B,
    0x8000000000008089,
    0x8000000000008003,
    0x8000000000008002,
    0x8000000000000080,
    0x000000000000800A,
    0x800000008000000A,
    0x8000000080008081,
    0x8000000000008080,
    0x0000000080000001,
    0x8000000080008008
  ]
