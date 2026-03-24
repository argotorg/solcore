module Solcore.Pipeline.SolcorePipeline where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum)
import Data.Time qualified as Time
import Language.Hull qualified as Hull
-- Pretty instances for MastCompUnit

import Solcore.Backend.EmitHull (emitHull)
import Solcore.Backend.Mast ()
import Solcore.Backend.MastEval (defaultFuel, eliminateDeadCode, evalCompUnit)
import Solcore.Backend.Specialise (specialiseCompUnit)
import Solcore.Desugarer.ContractDispatch (contractDispatchDesugarer)
import Solcore.Desugarer.DecisionTreeCompiler (matchCompiler, showWarning)
import Solcore.Desugarer.FieldAccess (fieldDesugarer)
import Solcore.Desugarer.IfDesugarer (ifDesugarer)
import Solcore.Desugarer.IndirectCall (indirectCall)
import Solcore.Desugarer.ReplaceFunTypeArgs
import Solcore.Desugarer.ReplaceWildcard (replaceWildcard)
import Solcore.Frontend.Module.Loader (ModuleGraph (..), flattenModuleStrictCompileCompUnitWithMetadata, flattenModuleStrictValidationCompUnit, loadModuleGraph, moduleSourcePath)
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax hiding (contracts)
import Solcore.Frontend.Syntax.NameResolution
import Solcore.Frontend.TypeInference.SccAnalysis
import Solcore.Frontend.TypeInference.TcContract
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Pipeline.Options (Option (..), argumentsParser, noDesugarOpt)
import System.Directory (makeAbsolute)
import System.Exit (ExitCode (..), exitWith)
import System.TimeIt qualified as TimeIt

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
      forM_ (zip [(1 :: Int) ..] contracts) $ \(i, c) -> do
        let filename = "output" <> show i <> ".hull"
        putStrLn ("Writing to " ++ filename)
        writeFile filename (show c)

-- Version that returns Either for testing
compile :: Option -> IO (Either String [Hull.Object])
compile opts = runExceptT $ do
  let verbose = optVerbose opts
      noDesugarCalls = optNoDesugarCalls opts
      noGenDispatch = optNoGenDispatch opts
      noMatchCompiler = optNoMatchCompiler opts
      noIfDesugar = optNoIfDesugar opts
      timeItNamed :: String -> IO a -> IO a
      timeItNamed = optTimeItNamed opts
      file = fileName opts
  mainRoot <- liftIO $ makeAbsolute (optRootDir opts)
  stdRoot <- ExceptT $ pure (parseStdRoot (optImportDirs opts))
  externalLibs <- ExceptT $ pure (parseExternalLibSpecs (optExternalLibs opts))

  -- Parsing and import loading
  graph <- ExceptT $ loadModuleGraph mainRoot stdRoot externalLibs file

  -- Validate each module against only its own direct imports.
  forM_ (moduleOrder graph) $ \moduleId -> do
    sourcePath <- ExceptT $ pure (moduleSourcePath graph moduleId)
    cunit <-
      ExceptT $
        pure (flattenModuleStrictValidationCompUnit graph moduleId)
    _ <-
      ExceptT $
        pure $
          first (\e -> "Module validation failed for " ++ sourcePath ++ ":\n" ++ e) $
            validateDuplicateNamespacesInCompUnit cunit
    _ <-
      ExceptT $
        first (\e -> "Module validation failed for " ++ sourcePath ++ ":\n" ++ e)
          <$> nameResolution cunit
    pure ()

  (parsed, localStart, importedStart, partialImportedTypeNames) <-
    ExceptT $
      pure (flattenModuleStrictCompileCompUnitWithMetadata graph (entryModule graph))

  -- Name resolution
  resolved <- ExceptT $ nameResolution parsed
  let CompUnit _ resolvedDecls = resolved
      trustedImportedInstanceHeads =
        [ instanceHeadKey inst
          | TInstDef inst <- drop importedStart resolvedDecls
        ]
      localDeclKeys =
        take (importedStart - localStart) (drop localStart resolvedDecls) >>= topDeclKeys

  liftIO $ when (verbose || optDumpAST opts) $ do
    putStrLn "> AST after name resolution"
    putStrLn $ pretty resolved

  -- contract field access desugaring
  let accessed = fieldDesugarer resolved
  liftIO $ when verbose $ do
    putStrLn "Contract field access desugaring:"
    putStrLn $ pretty accessed

  -- contract dispatch generation
  dispatched <-
    liftIO $
      if noGenDispatch
        then pure accessed
        else timeItNamed "Contract dispatch generation" $ pure (contractDispatchDesugarer accessed)

  liftIO $ when (optDumpDispatch opts) $ do
    putStrLn "> Dispatch:"
    putStrLn $ pretty dispatched

  -- SCC analysis
  connected <-
    ExceptT $
      timeItNamed "SCC           " $
        sccAnalysis dispatched

  liftIO $ when verbose $ do
    putStrLn "> SCC Analysis:"
    putStrLn $ pretty connected

  -- Indirect call handling
  direct <-
    liftIO $
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
    putStrLn "> Eliminating argments with function types"
    putStrLn $ pretty noFun

  -- Type inference, first round without any desugaring
  (_typed, _typedEnv) <-
    ExceptT $
      timeItNamed
        "Typecheck (no desugaring)  "
        (typeInferWithTrustedInstanceHeadsAndPartialTypes noDesugarOpt trustedImportedInstanceHeads localDeclKeys partialImportedTypeNames noFun)

  liftIO $ when verbose $ do
    putStrLn "No type errors found!"

  (typed, tcEnv) <-
    ExceptT $
      timeItNamed
        "Typecheck (desugaring)  "
        (typeInferWithTrustedInstanceHeadsAndPartialTypes opts trustedImportedInstanceHeads localDeclKeys partialImportedTypeNames noFun)

  liftIO $ when verbose $ do
    putStrLn "> Type inference logs:"
    mapM_ putStrLn (reverse $ logs tcEnv)
    putStrLn "> Elaborated tree:"
    putStrLn $ pretty typed

  -- If / boolean desugaring
  desugared <-
    liftIO $
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
      else do
        (ast, warns) <- ExceptT $ timeItNamed "Match compiler" $ matchCompiler desugared
        when (verbose && not (null warns)) $ liftIO $ mapM_ (putStrLn . showWarning) warns
        pure ast

  let printMatch = not noMatchCompiler && (verbose || optDumpDS opts)
  liftIO $ when printMatch $ do
    putStrLn "> Match compilation result:"
    putStrLn (pretty matchless)

  -- Specialization & Hull Generation
  if optNoSpec opts
    then pure []
    else do
      specialized <-
        liftIO $
          timeItNamed "Specialise    " $
            specialiseCompUnit matchless (optDebugSpec opts) tcEnv

      liftIO $ when (optDumpSpec opts) $ do
        putStrLn "> Specialised contract:"
        putStrLn (pretty specialized)

      let peFuel = maybe defaultFuel id (optPEFuel opts)
          (evaluated, remainingFuel) = evalCompUnit peFuel specialized

      liftIO $
        when (remainingFuel <= 0) $
          putStrLn "!! Warning: partial evaluation ran out of fuel (use --pe-fuel N to increase)"

      liftIO $ when (optDumpSpec opts) $ do
        putStrLn "> After partial evaluation:"
        putStrLn (pretty evaluated)

      -- Dead code elimination: remove functions unreachable from 'start'/'main'
      let optimized = eliminateDeadCode evaluated

      liftIO $ when (optDumpSpec opts) $ do
        putStrLn "> After dead code elimination:"
        putStrLn (pretty optimized)

      hull <-
        liftIO $
          timeItNamed "Emit Hull     " $
            emitHull (optDebugHull opts) optimized

      liftIO $ when (optDumpHull opts) $ do
        putStrLn "> Hull contract(s):"
        forM_ hull (putStrLn . pretty)

      pure hull

parseExternalLibSpecs :: [String] -> Either String [(Name, FilePath)]
parseExternalLibSpecs =
  fmap reverse . foldM step []
  where
    step acc spec = do
      (libName, libPath) <- splitSpec spec
      when (any ((== libName) . fst) acc) $
        Left ("Duplicate external library name: " ++ show libName)
      pure ((libName, libPath) : acc)

    splitSpec spec =
      case break (== '=') spec of
        (libNameStr, '=' : path)
          | null libNameStr || null path ->
              Left ("Invalid external library spec: " ++ spec)
          | not (validLibName libNameStr) ->
              Left ("Invalid external library name: " ++ libNameStr)
          | otherwise ->
              Right (Name libNameStr, path)
        _ ->
          Left ("Invalid external library spec: " ++ spec)

    validLibName [] = False
    validLibName (c : cs) =
      (isAlpha c || c == '_')
        && all (\ch -> isAlphaNum ch || ch == '_') cs

parseStdRoot :: String -> Either String (Maybe FilePath)
parseStdRoot spec =
  case filter (not . null) (splitColon spec) of
    [] -> Right Nothing
    [root] -> Right (Just root)
    _ ->
      Left "Multiple --include roots are no longer supported; use --lib for external libraries."
  where
    splitColon [] = []
    splitColon s =
      case break (== ':') s of
        (chunk, ':' : rest) -> chunk : splitColon rest
        (chunk, _) -> [chunk]

-- add declarations generated in the previous step
-- and moving data types inside contracts to the
-- global scope.
moveData :: CompUnit Name -> CompUnit Name
moveData (CompUnit imps decls1) =
  CompUnit imps (foldr step [] decls1)
  where
    step (TContr c) ac =
      let (dts, c') = extractData c
          dts' = map TDataDef dts
       in (TContr c') : dts' ++ ac
    step d ac = d : ac

extractData :: Contract Name -> ([DataTy], Contract Name)
extractData (Contract n ts ds) =
  (ds1, Contract n ts ds0)
  where
    (ds1, ds0) = foldr step ([], []) ds
    step (CDataDecl dt) (dts, cs) = (dt : dts, cs)
    step c (dts, cs) = (dts, c : cs)

addGenerated ::
  CompUnit Name ->
  [TopDecl Name] ->
  CompUnit Name
addGenerated (CompUnit imps ds) ts =
  CompUnit imps (ds ++ ts)

optTimeItNamed :: Option -> String -> IO a -> IO a
optTimeItNamed opts s a = if (optTiming opts) then TimeIt.timeItNamed s a else a
