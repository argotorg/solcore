module Solcore.Pipeline.SolcorePipeline where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)
import Data.IORef
import qualified Data.Time as Time
import System.Exit (ExitCode(..), exitWith)
import System.FilePath
import qualified System.TimeIt as TimeIt
import Text.Pretty.Simple

import qualified Language.Core as Core
import Solcore.Desugarer.IndirectCall (indirectCall)
import Solcore.Desugarer.LambdaLifting (lambdaLifting)
import Solcore.Desugarer.MatchCompiler (matchCompiler)
import Solcore.Desugarer.UniqueTypeGen (uniqueTypeGen)
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.SolcoreParser
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax.ElabTree
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax
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
      forM_ (zip [1..] contracts) $ \(i, c) -> do
        let filename = "output" <> show i <> ".core"
        putStrLn ("Writing to " ++ filename)
        writeFile filename (show c)

-- Version that returns Either for testing
compile :: Option -> IO (Either String [Core.Contract])
compile opts = runExceptT $ do
  let verbose = optVerbose opts
      noDesugarCalls = optNoDesugarCalls opts
      noMatchCompiler = optNoMatchCompiler opts
      timeItNamed :: String -> IO a -> IO a
      timeItNamed = optTimeItNamed opts
      file = fileName opts
      dir = takeDirectory file

  -- Parsing
  content <- liftIO $ readFile file

  parsed <- ExceptT $ moduleParser dir content
  (resolved, env) <- ExceptT $ buildAST' parsed

  liftIO $ when (verbose || optDumpAST opts) $ do
    putStrLn "> AST after name resolution"
    putStrLn $ pretty resolved

  liftIO $ when (optDumpEnv opts) $ pPrint env
    
  -- SCC analysis
  connected <- ExceptT $ timeItNamed "SCC           " $
    sccAnalysis resolved

  liftIO $ when verbose $ do
    putStrLn "> SCC Analysis:"
    putStrLn $ pretty connected

  -- Indirect call handling
  direct <- liftIO $
    if noDesugarCalls
    then pure connected
    else timeItNamed "Indirect Calls" $ (fst <$> indirectCall connected)

  liftIO $ when verbose $ do
    putStrLn "> Indirect call desugaring:"
    putStrLn $ pretty direct

  -- Type inference
  (typed, env) <- ExceptT $ timeItNamed
    (if noDesugarCalls then "Indirect Calls" else "Typecheck     ")
    (typeInfer opts direct)

  liftIO $ when verbose $ do
    putStrLn "> Type inference logs:"
    mapM_ putStrLn (reverse $ logs env)
    putStrLn "> Elaborated tree:"
    putStrLn $ pretty typed

  -- Match compilation
  matchless <-
    if noMatchCompiler
    then pure typed
    else ExceptT $ timeItNamed "Match compiler" $ matchCompiler typed
  let printMatch = (not $ noMatchCompiler) && (verbose || optDumpDS opts)
  liftIO $ when printMatch $ do
    putStrLn "> Match compilation result:"
    putStrLn (pretty matchless)

  -- Specialization & Core Generation
  if optNoSpec opts
  then pure []
  else do
    specialized <- liftIO $ timeItNamed "Specialise    " $
      specialiseCompUnit matchless (optDebugSpec opts) env

    liftIO $ when (optDumpSpec opts) $ do
      putStrLn "> Specialised contract:"
      putStrLn (pretty specialized)

    core <- liftIO $ timeItNamed "Emit Core     " $
      emitCore (optDebugCore opts) env specialized

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
