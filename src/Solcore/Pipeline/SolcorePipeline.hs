module Solcore.Pipeline.SolcorePipeline where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.List.Split(splitOn)
import qualified Data.Time as Time
import Solcore.Desugarer.IfDesugarer (ifDesugarer)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath
import qualified System.TimeIt as TimeIt
import Text.Pretty.Simple

import qualified Language.Core as Core
import Solcore.Desugarer.ContractDispatch (contractDispatchDesugarer)
import Solcore.Desugarer.FieldAccess(fieldDesugarer)
import Solcore.Desugarer.IndirectCall (indirectCall)
import Solcore.Desugarer.MatchCompiler (matchCompiler)
import Solcore.Desugarer.ReplaceWildcard (replaceWildcard)
import Solcore.Desugarer.ReplaceFunTypeArgs
import Solcore.Frontend.Parser.SolcoreParser
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax.ElabTree
import Solcore.Frontend.Syntax.Contract hiding(contracts)
import Solcore.Frontend.Syntax hiding(contracts)
import Solcore.Frontend.TypeInference.SccAnalysis
import Solcore.Frontend.TypeInference.TcContract
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Desugarer.Specialise(specialiseCompUnit)
import Solcore.Desugarer.EmitCore(emitCore)
import Solcore.Pipeline.Options(Option(..), argumentsParser)

-- main compiler driver function
pipeline :: IO ()
pipeline = do
  _startTime <- Time.getCurrentTime
  opts <- argumentsParser
  result <- compile opts
  case result of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 1)
    Right contracts -> do
      forM_ (zip [(1::Int)..] contracts) $ \(i, c) -> do
        let filename = "output" <> show i <> ".core"
        putStrLn ("Writing to " ++ filename)
        writeFile filename (show c)

-- Version that returns Either for testing
compile :: Option -> IO (Either String [Core.Object])
compile opts = runExceptT $ do
  let verbose = optVerbose opts
      noDesugarCalls = optNoDesugarCalls opts
      noGenDispatch = optNoGenDispatch opts
      noMatchCompiler = optNoMatchCompiler opts
      noIfDesugar = optNoIfDesugar opts
      timeItNamed :: String -> IO a -> IO a
      timeItNamed = optTimeItNamed opts
      file = fileName opts
      dir = takeDirectory file
      otherDirs = splitOn ":" (optImportDirs opts)
      dirs = dir:otherDirs

  -- Parsing
  content <- liftIO $ readFile file

  parsed <- ExceptT $ moduleParser dirs content

  -- Name resolution
  (resolved, nameEnv) <- ExceptT $ buildAST' parsed

  liftIO $ when (verbose || optDumpAST opts) $ do
    putStrLn "> AST after name resolution"
    putStrLn $ pretty resolved

  liftIO $ when (optDumpEnv opts) $ pPrint nameEnv

  -- contract field access desugaring
  let accessed = fieldDesugarer resolved
  liftIO $ when verbose $ do
    putStrLn "Contract field access desugaring:"
    putStrLn $ pretty accessed

  -- contract dispatch generation
  dispatched <- liftIO $
    if noGenDispatch
    then pure accessed
    else timeItNamed "Contract dispatch generation" $ pure (contractDispatchDesugarer accessed)

  liftIO $ when (optDumpDispatch opts) $ do
    putStrLn "> Dispatch:"
    putStrLn $ pretty dispatched

  -- SCC analysis
  connected <- ExceptT $ timeItNamed "SCC           " $
    sccAnalysis dispatched

  liftIO $ when verbose $ do
    putStrLn "> SCC Analysis:"
    putStrLn $ pretty connected

  -- Indirect call handling
  direct <- liftIO $
    if noDesugarCalls
    then pure connected
    else timeItNamed "Indirect Calls" $ (fst <$> indirectCall connected)

  liftIO $ when (verbose || optDumpDF opts) $ do
    putStrLn "> Indirect call desugaring:"
    putStrLn $ pretty direct

  -- Pattern wildcard desugaring

  let noWild = replaceWildcard direct
  liftIO $ when verbose $ do
    putStrLn "> Pattern wildcard desugaring:"
    putStrLn $ pretty noWild

  -- Eliminate function type arguments

  let noFun = if noDesugarCalls then noWild else replaceFunParam noWild
  liftIO $ when verbose $ do
    putStrLn "> Eliminating arguments with function types"
    putStrLn $ pretty noFun

  -- Type inference
  (typed, typeEnv) <- ExceptT $ timeItNamed "Typecheck     "
    (typeInfer opts noFun)

  liftIO $ when verbose $ do
    putStrLn "> Type inference logs:"
    mapM_ putStrLn (reverse $ logs typeEnv)
    putStrLn "> Elaborated tree:"
    putStrLn $ pretty typed

  -- If / boolean desugaring
  desugared <- liftIO $
    if noIfDesugar
    then pure typed
    else timeItNamed "If/Bool desugaring" (pure (ifDesugarer typed))

  liftIO $ when verbose $ do
    putStrLn "> If / Bool desugaring:"
    putStrLn $ pretty desugared

  -- Match compilation
  matchless <-
    if noMatchCompiler
    then pure desugared
    else ExceptT $ timeItNamed "Match compiler" $ matchCompiler desugared

  let printMatch = (not $ noMatchCompiler) && (verbose || optDumpDS opts)
  liftIO $ when printMatch $ do
    putStrLn "> Match compilation result:"
    putStrLn (pretty matchless)

  -- Specialization & Core Generation
  if optNoSpec opts
  then pure []
  else do
    specialized <- liftIO $ timeItNamed "Specialise    " $
      specialiseCompUnit matchless (optDebugSpec opts) typeEnv

    liftIO $ when (optDumpSpec opts) $ do
      putStrLn "> Specialised contract:"
      putStrLn (pretty specialized)

    core <- liftIO $ timeItNamed "Emit Core     " $
      emitCore (optDebugCore opts) typeEnv specialized

    liftIO $ when (optDumpCore opts) $ do
      putStrLn "> Core contract(s):"
      forM_ core (putStrLn . pretty)

    pure core

-- add declarations generated in the previous step
-- and moving data types inside contracts to the
-- global scope.
moveData :: CompUnit Name -> CompUnit Name
moveData (CompUnit imps decls1)
  = CompUnit imps (foldr step [] decls1)
    where
      step (TContr c) ac
        = let (dts, c') = extractData c
              dts' = map TDataDef dts
          in (TContr c') : dts' ++ ac
      step d ac = d : ac

extractData :: Contract Name -> ([DataTy], Contract Name)
extractData (Contract n ts ds)
  = (ds1, Contract n ts ds0)
    where
      (ds1, ds0) = foldr step ([], []) ds
      step (CDataDecl dt) (dts, cs) = (dt : dts, cs)
      step c (dts, cs) = (dts, c : cs)

addGenerated :: CompUnit Name ->
                [TopDecl Name] ->
                CompUnit Name
addGenerated (CompUnit imps ds) ts
  = CompUnit imps (ds ++ ts)

optTimeItNamed :: Option -> String -> IO a -> IO a
optTimeItNamed opts s a = if (optTiming opts) then TimeIt.timeItNamed s a else a
