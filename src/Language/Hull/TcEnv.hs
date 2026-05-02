module Language.Hull.TcEnv
  ( HullFunSig (..),
    HullTcEnv (..),
    emptyHullTcEnv,
    hullBuiltins,
    returnCount,
    nReturns,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Language.Hull.Types

data HullFunSig = HullFunSig
  { hsig_args :: [Type],
    hsig_ret :: Type
  }

data HullTcEnv = HullTcEnv
  { hull_vars :: Map String Type,
    hull_funs :: Map String HullFunSig,
    hull_ret :: Maybe Type
  }

emptyHullTcEnv :: HullTcEnv
emptyHullTcEnv = HullTcEnv Map.empty hullBuiltins Nothing

-- Number of word-sized slots a return type occupies.
returnCount :: Type -> Int
returnCount TUnit = 0
returnCount TWord = 1
returnCount (TPair a b) = returnCount a + returnCount b
returnCount _ = 1

-- Return type for exactly n word-sized return slots.
nReturns :: Int -> Type
nReturns 0 = TUnit
nReturns 1 = TWord
nReturns n = TPair TWord (nReturns (n - 1))

-- All EVM built-in functions and Yul object built-ins available in Hull.
hullBuiltins :: Map String HullFunSig
hullBuiltins =
  Map.fromList
    [ ("stop", HullFunSig [] TUnit),
      ("invalid", HullFunSig [] TUnit),
      ("add", HullFunSig w2 TWord),
      ("sub", HullFunSig w2 TWord),
      ("mul", HullFunSig w2 TWord),
      ("div", HullFunSig w2 TWord),
      ("sdiv", HullFunSig w2 TWord),
      ("mod", HullFunSig w2 TWord),
      ("smod", HullFunSig w2 TWord),
      ("exp", HullFunSig w2 TWord),
      ("signextend", HullFunSig w2 TWord),
      ("lt", HullFunSig w2 TWord),
      ("gt", HullFunSig w2 TWord),
      ("slt", HullFunSig w2 TWord),
      ("sgt", HullFunSig w2 TWord),
      ("eq", HullFunSig w2 TWord),
      ("and", HullFunSig w2 TWord),
      ("or", HullFunSig w2 TWord),
      ("xor", HullFunSig w2 TWord),
      ("byte", HullFunSig w2 TWord),
      ("shl", HullFunSig w2 TWord),
      ("shr", HullFunSig w2 TWord),
      ("sar", HullFunSig w2 TWord),
      ("iszero", HullFunSig w1 TWord),
      ("not", HullFunSig w1 TWord),
      ("keccak256", HullFunSig w2 TWord),
      ("addmod", HullFunSig w3 TWord),
      ("mulmod", HullFunSig w3 TWord),
      ("address", HullFunSig [] TWord),
      ("selfbalance", HullFunSig [] TWord),
      ("caller", HullFunSig [] TWord),
      ("callvalue", HullFunSig [] TWord),
      ("calldatasize", HullFunSig [] TWord),
      ("codesize", HullFunSig [] TWord),
      ("returndatasize", HullFunSig [] TWord),
      ("origin", HullFunSig [] TWord),
      ("gasprice", HullFunSig [] TWord),
      ("coinbase", HullFunSig [] TWord),
      ("timestamp", HullFunSig [] TWord),
      ("number", HullFunSig [] TWord),
      ("prevrandao", HullFunSig [] TWord),
      ("difficulty", HullFunSig [] TWord),
      ("gaslimit", HullFunSig [] TWord),
      ("chainid", HullFunSig [] TWord),
      ("basefee", HullFunSig [] TWord),
      ("blobbasefee", HullFunSig [] TWord),
      ("gas", HullFunSig [] TWord),
      ("pc", HullFunSig [] TWord),
      ("msize", HullFunSig [] TWord),
      ("balance", HullFunSig w1 TWord),
      ("extcodesize", HullFunSig w1 TWord),
      ("extcodehash", HullFunSig w1 TWord),
      ("blockhash", HullFunSig w1 TWord),
      ("calldataload", HullFunSig w1 TWord),
      ("mload", HullFunSig w1 TWord),
      ("sload", HullFunSig w1 TWord),
      ("tload", HullFunSig w1 TWord),
      ("mstore", HullFunSig w2 TUnit),
      ("mstore8", HullFunSig w2 TUnit),
      ("sstore", HullFunSig w2 TUnit),
      ("tstore", HullFunSig w2 TUnit),
      ("return", HullFunSig w2 TUnit),
      ("revert", HullFunSig w2 TUnit),
      ("calldatacopy", HullFunSig w3 TUnit),
      ("codecopy", HullFunSig w3 TUnit),
      ("returndatacopy", HullFunSig w3 TUnit),
      ("mcopy", HullFunSig w3 TUnit),
      ("extcodecopy", HullFunSig w3 TUnit),
      ("create", HullFunSig w3 TUnit),
      ("create2", HullFunSig w3 TUnit),
      ("call", HullFunSig w7 TWord),
      ("callcode", HullFunSig w7 TWord),
      ("delegatecall", HullFunSig w6 TWord),
      ("staticcall", HullFunSig w6 TWord),
      ("log0", HullFunSig w2 TUnit),
      ("log1", HullFunSig w3 TUnit),
      ("log2", HullFunSig w4 TUnit),
      ("log3", HullFunSig [TWord, TWord, TWord, TWord, TWord] TUnit),
      ("log4", HullFunSig [TWord, TWord, TWord, TWord, TWord, TWord] TUnit),
      ("pop", HullFunSig w1 TUnit),
      ("selfdestruct", HullFunSig w1 TUnit),
      ("jump", HullFunSig w1 TUnit),
      ("jumpi", HullFunSig w2 TUnit),
      ("memoryguard", HullFunSig w1 TWord),
      ("dataoffset", HullFunSig w1 TWord),
      ("datasize", HullFunSig w1 TWord),
      ("loadimmutable", HullFunSig w1 TWord),
      ("setimmutable", HullFunSig w3 TUnit),
      ("linkersymbol", HullFunSig w1 TWord)
    ]
  where
    w1 = [TWord]
    w2 = [TWord, TWord]
    w3 = [TWord, TWord, TWord]
    w4 = [TWord, TWord, TWord, TWord]
    w6 = [TWord, TWord, TWord, TWord, TWord, TWord]
    w7 = [TWord, TWord, TWord, TWord, TWord, TWord, TWord]
