{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Csol.Exec
  ( encodeCalldata,
    runCreate,
    runCall,
    runDirect,
    ExecResult (..),
    StateDiff (..),
    AccountDiff (..),
    diffState,
    emptyContracts,
    vmContracts,
    deployAddress,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.ST (stToIO)
import Control.Monad.Trans.State.Strict (execStateT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Word (Word64)
import EVM (initialContract, makeVm)
import EVM.Concrete (createAddress)
import EVM.Effects (defaultConfig, runApp)
import EVM.Exec (ethrunAddress, exec)
import EVM.FeeSchedule (feeSchedule)
import EVM.Solidity qualified as Solidity
import EVM.SymExec (symCalldata)
import EVM.Types
import System.Exit (die)

-- | Encode a function call as ABI calldata (4-byte selector + encoded args).
-- Uses hevm's functionAbi (which calls solc) to parse the signature,
-- then symCalldata to produce the encoded buffer.
--
-- We pass ConcreteBuf "" as the base buffer so that concrete argument values
-- are written into a concrete buffer (Lit writes into ConcreteBuf stay concrete).
-- mkCalldata uses AbstractBuf "txdata" as the base, which causes all writes to
-- produce symbolic WriteWord/WriteByte expressions even for concrete arguments.
encodeCalldata :: String -> [String] -> IO ByteString
encodeCalldata sig args = runApp $ do
  method <- liftIO $ Solidity.functionAbi (pack sig)
  (buf, _) <- symCalldata method.methodSignature (snd <$> method.inputs) args (ConcreteBuf "")
  case buf of
    ConcreteBuf bs -> pure bs
    _ -> liftIO $ die "encodeCalldata: expected concrete calldata but got symbolic expression"

data ExecResult = ExecResult
  { erSuccess :: Bool,
    erOutput :: ByteString,
    erGasUsed :: Word64,
    erError :: Maybe EvmError
  }
  deriving (Show)

mkVMOpts ::
  Contract ->
  [(Expr EAddr, Contract)] ->
  (Expr Buf, [Prop]) ->
  Expr EWord ->
  Expr EAddr ->
  Bool ->
  VMOpts Concrete
mkVMOpts c others cd val addr isCreate =
  VMOpts
    { contract = c,
      otherContracts = others,
      calldata = cd,
      value = val,
      baseState = EmptyBase,
      address = addr,
      caller = LitAddr ethrunAddress,
      origin = LitAddr ethrunAddress,
      coinbase = LitAddr 0,
      number = Lit 0,
      timestamp = Lit 0,
      blockGaslimit = 0,
      gasprice = 0,
      prevRandao = 42069,
      gas = 0xffffffffffffffff,
      gaslimit = 0xffffffffffffffff,
      baseFee = 0,
      priorityFee = 0,
      maxCodeSize = 0xffffffff,
      schedule = feeSchedule,
      chainId = 1,
      create = isCreate,
      txAccessList = mempty,
      allowFFI = False,
      freshAddresses = 0,
      beaconRoot = 0
    }

runVM :: VM Concrete -> IO (VM Concrete)
runVM vm0 = stToIO $ execStateT (exec defaultConfig) vm0

-- | Run contract creation (initcode execution).
runCreate :: ByteString -> Maybe ByteString -> Maybe W256 -> IO (ExecResult, VM Concrete)
runCreate initcode mArgs mValue = do
  let code = initcode <> fromMaybe mempty mArgs
      val = maybe (Lit 0) Lit mValue
      deployAddr = createAddress ethrunAddress 1
      opts =
        mkVMOpts
          (initialContract (InitCode code mempty))
          [(LitAddr ethrunAddress, initialContract (RuntimeCode (ConcreteRuntimeCode "")))]
          (ConcreteBuf mempty, [])
          val
          deployAddr
          True
  vm0 <- stToIO $ makeVm opts
  vm1 <- runVM vm0
  pure (extractResult vm1, vm1)

-- | Run a call against a deployed contract using state from a previous create.
runCall :: VM Concrete -> Maybe ByteString -> Maybe W256 -> IO (ExecResult, VM Concrete)
runCall postCreateVM mCalldata mValue = do
  let deployAddr = createAddress ethrunAddress 1
      cd = fromMaybe mempty mCalldata
      val = maybe (Lit 0) Lit mValue
      contracts = postCreateVM.env.contracts
  case Map.lookup deployAddr contracts of
    Nothing -> die "runCall: deployed contract not found at expected address"
    Just deployed -> do
      let opts =
            mkVMOpts
              deployed
              (Map.toList (Map.delete deployAddr contracts))
              (ConcreteBuf cd, [])
              val
              deployAddr
              False
      vm0 <- stToIO $ makeVm opts
      vm1 <- runVM vm0
      pure (extractResult vm1, vm1)

-- | Run bytecode directly without a create phase.
runDirect :: ByteString -> Maybe ByteString -> Maybe W256 -> IO (ExecResult, VM Concrete)
runDirect bytecode mCalldata mValue = do
  let cd = fromMaybe mempty mCalldata
      val = maybe (Lit 0) Lit mValue
      addr = createAddress ethrunAddress 1
      opts =
        mkVMOpts
          (initialContract (RuntimeCode (ConcreteRuntimeCode bytecode)))
          [(LitAddr ethrunAddress, initialContract (RuntimeCode (ConcreteRuntimeCode "")))]
          (ConcreteBuf cd, [])
          val
          addr
          False
  vm0 <- stToIO $ makeVm opts
  vm1 <- runVM vm0
  pure (extractResult vm1, vm1)

-- | Extract execution result from a finished VM.
extractResult :: VM Concrete -> ExecResult
extractResult vm = case vm.result of
  Just (VMSuccess (ConcreteBuf bs)) ->
    ExecResult True bs vm.burned Nothing
  Just (VMSuccess _) ->
    ExecResult True mempty vm.burned Nothing
  Just (VMFailure e) ->
    ExecResult False mempty vm.burned (Just e)
  _ ->
    ExecResult False mempty vm.burned Nothing

-- | Extract the contracts map from a VM state.
vmContracts :: VM Concrete -> Contracts
vmContracts vm = vm.env.contracts

-- | The address where the contract gets deployed.
deployAddress :: Expr EAddr
deployAddress = createAddress ethrunAddress 1

-- State diffing -----------------------------------------------------------

type Contracts = Map.Map (Expr EAddr) Contract

-- | An empty contract map (used as the "before" state for the create phase).
emptyContracts :: Contracts
emptyContracts = Map.empty

-- | Diff for a single account.
data AccountDiff = AccountDiff
  { adBalance :: Maybe (W256, W256), -- (old, new) if changed
    adNonce :: Maybe (Maybe W64, Maybe W64),
    adStorage :: [(W256, Maybe W256, W256)], -- (slot, old, new) — old is Nothing for new slots
    adNewCode :: Bool, -- account was created
    adCodeSize :: Int -- size of deployed code in bytes
  }
  deriving (Show)

-- | Overall state diff.
data StateDiff = StateDiff
  { sdAccounts :: [(Expr EAddr, AccountDiff)]
  }
  deriving (Show)

-- | Compute the diff between two contract maps.
diffState :: Contracts -> Contracts -> StateDiff
diffState before after =
  StateDiff
    { sdAccounts = concatMap diffAddr (Map.toList after)
    }
  where
    diffAddr (addr, post) =
      let mPre = Map.lookup addr before
          isNew = case mPre of Nothing -> True; Just _ -> False
          preBalance = maybe 0 exprToW256 (fmap (.balance) mPre)
          postBalance = exprToW256 post.balance
          balDiff =
            if preBalance /= postBalance
              then Just (preBalance, postBalance)
              else Nothing
          preNonce = maybe Nothing (.nonce) mPre
          postNonce = post.nonce
          nonceDiff =
            if preNonce /= postNonce
              then Just (preNonce, postNonce)
              else Nothing
          storageDiff = diffStorage (maybe Map.empty getStorage mPre) (getStorage post)
          codeSize = getCodeSize post
          ad = AccountDiff balDiff nonceDiff storageDiff isNew codeSize
       in if isNew || balDiff /= Nothing || nonceDiff /= Nothing || not (null storageDiff)
            then [(addr, ad)]
            else []

    getCodeSize :: Contract -> Int
    getCodeSize c = case c.code of
      RuntimeCode (ConcreteRuntimeCode bs) -> BS.length bs
      _ -> 0

    getStorage :: Contract -> Map.Map W256 W256
    getStorage c = case c.storage of
      ConcreteStore m -> m
      _ -> Map.empty

    exprToW256 :: Expr EWord -> W256
    exprToW256 (Lit w) = w
    exprToW256 _ = 0

    diffStorage :: Map.Map W256 W256 -> Map.Map W256 W256 -> [(W256, Maybe W256, W256)]
    diffStorage pre post =
      let allKeys = Map.keys (Map.union pre post)
       in concatMap
            ( \k ->
                let old = Map.lookup k pre
                    new = Map.lookup k post
                 in case (old, new) of
                      (Nothing, Just v) -> [(k, Nothing, v)]
                      (Just o, Just v) | o /= v -> [(k, Just o, v)]
                      _ -> []
            )
            allKeys
