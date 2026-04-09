module Csol.Run (execute) where

import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as BS16
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text qualified as T
import EVM.Format (formatExpr, hexByteString, strip0x)
import Numeric (showHex)
import System.Exit (exitWith, ExitCode(..), die)
import Text.PrettyPrint hiding ((<>))

import Csol.Build (buildToBytes)
import Csol.BuildOpts
import Csol.Exec
import Csol.RunOpts (RunOpts(..), CalldataSpec(..))

import Options qualified as Yule

execute :: BuildOpts -> RunOpts -> IO ()
execute buildOpts runOpts = do
  let buildOpts' = if roCreate runOpts then buildOpts
        else buildOpts { boYule = (boYule buildOpts) { Yule.runOnce = True } }
  bytecode <- buildToBytes buildOpts'
  putStrLn ""
  if roCreate runOpts then do
    createData <- encodeCallSpec (roCreateCalldata runOpts)
    let createVal = roCreateCallvalue runOpts

    (createRes, postCreateVM) <- runCreate bytecode createData createVal
    putStrLn $ render $ ppCreate createRes
    putStrLn $ render $ ppDiff (diffState emptyContracts (vmContracts postCreateVM))

    when (not (erSuccess createRes)) $
      exitWith (ExitFailure 1)

    putStrLn ""
    runtimeData <- encodeCallSpec (roRuntimeCalldata runOpts)
    let runtimeVal = roRuntimeCallvalue runOpts
    let preContracts = vmContracts postCreateVM

    (callRes, postCallVM) <- runCall postCreateVM runtimeData runtimeVal
    putStrLn $ render $ ppRuntime callRes
    putStrLn $ render $ ppDiff (diffState preContracts (vmContracts postCallVM))
  else do
    runtimeData <- encodeCallSpec (roRuntimeCalldata runOpts)
    let runtimeVal = roRuntimeCallvalue runOpts
    (res, postVM) <- runDirect bytecode runtimeData runtimeVal
    putStrLn $ render $ ppRuntime res
    putStrLn $ render $ ppDiff (diffState emptyContracts (vmContracts postVM))

encodeCallSpec :: CalldataSpec -> IO (Maybe BS.ByteString)
encodeCallSpec (AbiCall sig args) = Just <$> encodeCalldata sig args
encodeCallSpec (RawHex hex)       = Just <$> decodeHexStr hex
encodeCallSpec NoCalldata         = pure Nothing

-- | Decode hex string (with optional 0x prefix) to ByteString.
decodeHexStr :: String -> IO BS.ByteString
decodeHexStr s =
  case hexByteString (strip0x (encodeUtf8 (T.pack s))) of
    Just bs -> pure bs
    Nothing -> die $ "Invalid hex: " <> s

-- Pretty printers ---------------------------------------------------------

ppCreate :: ExecResult -> Doc
ppCreate res =
  text "Create:" <+> text status
  $$ nest 2 (text "Gas used:" <+> text (show (erGasUsed res)))
  $$ nest 2 (text "Address: " <+> text (T.unpack (formatExpr deployAddress)))
  $$ maybe empty (\e -> nest 2 (text "Reason:  " <+> text (show e))) (erError res)
  where status = if erSuccess res then "success" else "failure"

ppRuntime :: ExecResult -> Doc
ppRuntime res =
  text "Runtime:" <+> text status
  $$ nest 2 (text "Gas used:" <+> text (show (erGasUsed res)))
  $$ if BS.null (erOutput res) then empty
     else nest 2 (text "Output:  " <+> text ("0x" <> T.unpack (decodeUtf8 (BS16.encode (erOutput res)))))
  $$ maybe empty (\e -> nest 2 (text "Reason:  " <+> text (show e))) (erError res)
  where status = if erSuccess res then "success" else "failure"

ppDiff :: StateDiff -> Doc
ppDiff sd
  | null (sdAccounts sd) = empty
  | otherwise =
      nest 2 (text "State changes:"
        $$ nest 2 (vcat (map ppAccount (sdAccounts sd))))
  where
    ppAccount (addr, ad) =
      text (T.unpack (formatExpr addr)) <> colon
      $$ nest 2 (vcat $ filter (not . isEmpty)
        [ if adNewCode ad
          then text "code:    0 ->" <+> text (show (adCodeSize ad)) <+> text "bytes"
          else empty
        , case adNonce ad of
            Just (old, new) -> text "nonce:  " <+> text (showMaybeW64 old) <+> text "->" <+> text (showMaybeW64 new)
            Nothing -> empty
        , case adBalance ad of
            Just (old, new) -> text "balance:" <+> text (showW old) <+> text "->" <+> text (showW new)
            Nothing -> empty
        , if null (adStorage ad) then empty
          else text "storage:" $$ nest 2 (vcat (map ppSlot (adStorage ad)))
        ])

    ppSlot (slot, mOld, new) = case mOld of
      Nothing  -> text (showW slot ++ ":") <+> text "0 ->" <+> text (showW new)
      Just old -> text (showW slot ++ ":") <+> text (showW old) <+> text "->" <+> text (showW new)

    showW w = "0x" ++ showH w
    showH w = showHex w ""
    showMaybeW64 Nothing  = "0"
    showMaybeW64 (Just w) = show w
