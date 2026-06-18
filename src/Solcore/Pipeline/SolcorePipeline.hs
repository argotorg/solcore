module Solcore.Pipeline.SolcorePipeline where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Time qualified as Time
import Language.Hull qualified as Hull
-- Pretty instances for MastCompUnit

import Solcore.Backend.ComptimeCheck (checkComptime)
import Solcore.Backend.EmitHull (emitHull)
import Solcore.Backend.Mast ()
import Solcore.Backend.MastEval (defaultFuel, eliminateDeadCode, evalCompUnit)
import Solcore.Backend.Specialise (specialiseCompUnit)
import Solcore.Desugarer.ContractDispatch (contractDispatchTopDecls)
import Solcore.Desugarer.DecisionTreeCompiler (matchCompiler, showWarning)
import Solcore.Desugarer.DeriveGeneric (deriveGenericTopDecls)
import Solcore.Desugarer.FieldAccess (fieldDesugarTopDecls)
import Solcore.Desugarer.IfDesugarer (ifDesugarer)
import Solcore.Desugarer.IndirectCall (indirectCallTopDecls)
import Solcore.Desugarer.IntLiteralDesugar (desugarIntLiterals)
import Solcore.Desugarer.ReplaceFunTypeArgs
import Solcore.Desugarer.ReplaceWildcard (replaceWildcardTopDecls)
import Solcore.Frontend.ComptimeCheck (checkComptimeEarly)
import Solcore.Frontend.Module.Identity qualified as Mod
import Solcore.Frontend.Module.Loader (ModuleGraph (..), loadModuleGraph, moduleSourcePath, moduleValidationTopDeclSegments)
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax hiding (contracts)
import Solcore.Frontend.Syntax.NameResolution
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.SccAnalysis
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcModule
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
    (validationImports, validationSegments) <-
      ExceptT $
        pure (moduleValidationTopDeclSegments graph moduleId)
    _ <-
      ExceptT $
        pure $
          first (\e -> "Module validation failed for " ++ sourcePath ++ ":\n" ++ e) $
            validateDuplicateNamespacesInTopDeclSegments validationSegments
    _ <-
      ExceptT $
        first (\e -> "Module validation failed for " ++ sourcePath ++ ":\n" ++ e)
          <$> nameResolutionTopDeclSegments validationImports validationSegments
    pure ()

  checkedModules <-
    ExceptT $
      timeItNamed "Typecheck modules" $
        runExceptT (typeCheckLoadedModules opts graph)
  checkedAssembly <- ExceptT $ pure (assembleCheckedModules graph checkedModules)
  let typed = checkedAssemblyCompUnit checkedAssembly
      tcEnv = checkedAssemblyEnv checkedAssembly

  -- SAIL-level comptime verification
  ExceptT $ return $ checkComptimeEarly typed

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

      evaluated <- liftIO $ timeItNamed "Comptime eval " $ do
        let peFuel = maybe defaultFuel id (optPEFuel opts)
            (evalResult, remainingFuel) = evalCompUnit peFuel specialized

        liftIO $
          when (remainingFuel <= 0) $
            putStrLn "!! Warning: partial evaluation ran out of fuel (use --pe-fuel N to increase)"

        liftIO $ when (optDumpSpec opts) $ do
          putStrLn "> After partial evaluation:"
          putStrLn (pretty evalResult)

        pure evalResult

      -- Dead code elimination: remove functions unreachable from 'start'/'main'
      let optimized = eliminateDeadCode evaluated

      liftIO $ when (optDumpSpec opts) $ do
        putStrLn "> After dead code elimination:"
        putStrLn (pretty optimized)

      -- Comptime verification: check comptime annotations are satisfied
      ExceptT $ return $ checkComptime optimized

      hull <-
        liftIO $
          timeItNamed "Emit Hull     " $
            emitHull (optDebugHull opts) optimized

      liftIO $ when (optDumpHull opts) $ do
        putStrLn "> Hull contract(s):"
        forM_ hull (putStrLn . pretty)

      pure hull

typeCheckLoadedModules :: Option -> ModuleGraph -> ExceptT String IO (Map Mod.ModuleId CheckedModule)
typeCheckLoadedModules opts graph =
  Map.fromList <$> mapM (typeCheckModuleFromGraph opts graph) (moduleOrder graph)

typeCheckModuleFromGraph ::
  Option ->
  ModuleGraph ->
  Mod.ModuleId ->
  ExceptT String IO (Mod.ModuleId, CheckedModule)
typeCheckModuleFromGraph opts graph moduleId = do
  sourcePath <- ExceptT $ pure (moduleSourcePath graph moduleId)
  resolvedInput <-
    ExceptT $
      first (moduleTypeCheckError sourcePath "input") <$> loadModuleLocalTypeCheckInput graph moduleId
  liftIO $ dumpModuleResolvedAST opts sourcePath resolvedInput
  moduleInput <- prepareModuleTypeCheckInput opts resolvedInput
  (noDesugarChecked, _noDesugarEnv) <-
    ExceptT $
      first (moduleTypeCheckError sourcePath "no desugaring") <$> typeInferModuleLocals noDesugarOpt moduleInput
  liftIO $
    when (optVerbose opts) $
      putStrLn ("No type errors found for " ++ sourcePath ++ "!")
  (typed, tcEnv) <-
    ExceptT $
      first (moduleTypeCheckError sourcePath "desugaring") <$> typeInferModuleLocals opts moduleInput
  liftIO $ dumpModuleTypeInference opts sourcePath typed tcEnv
  pure
    ( moduleId,
      CheckedModule
        { checkedModuleId = moduleId,
          checkedModuleInput = moduleInput,
          checkedModuleNoDesugar = noDesugarChecked,
          checkedModuleTyped = typed,
          checkedModuleEnv = tcEnv
        }
    )

prepareModuleTypeCheckInput ::
  Option ->
  ModuleResolvedTypeCheckInput ->
  ExceptT String IO ModuleTypeCheckInput
prepareModuleTypeCheckInput opts resolvedInput = do
  inferenceDecls <- prepareModuleInferenceDeclsForTypeInference opts resolvedInput
  pure (withPreparedModuleInferenceDecls resolvedInput inferenceDecls)

prepareModuleInferenceDeclsForTypeInference ::
  Option ->
  ModuleResolvedTypeCheckInput ->
  ExceptT String IO [ModuleInferenceDecl]
prepareModuleInferenceDeclsForTypeInference opts input =
  prepareInferenceDeclsForTypeInference
    opts
    (emitModulePreparationDiagnostics opts)
    (moduleResolvedImports input)
    (moduleInitialInferenceDecls input)

dumpModuleResolvedAST :: Option -> FilePath -> ModuleResolvedTypeCheckInput -> IO ()
dumpModuleResolvedAST opts sourcePath input =
  when (optVerbose opts || optDumpAST opts) $ do
    putStrLn ("> AST after name resolution for " ++ sourcePath)
    putStrLn $ pretty dumpCompUnit
  where
    dumpCompUnit =
      CompUnit
        (moduleResolvedImports input)
        ( moduleResolvedQualifiedDecls input
            ++ moduleResolvedLocalDecls input
            ++ moduleResolvedImportedDecls input
        )

emitModulePreparationDiagnostics :: Option -> Bool
emitModulePreparationDiagnostics opts =
  or
    [ optVerbose opts,
      optDumpDispatch opts,
      optDumpDF opts
    ]

dumpModuleTypeInference :: Option -> FilePath -> CompUnit Id -> TcEnv -> IO ()
dumpModuleTypeInference opts sourcePath typed tcEnv =
  when (optVerbose opts) $ do
    putStrLn ("> Type inference logs for " ++ sourcePath ++ ":")
    mapM_ putStrLn (reverse $ logs tcEnv)
    putStrLn ("> Elaborated tree for " ++ sourcePath ++ ":")
    putStrLn $ pretty typed

moduleTypeCheckError :: FilePath -> String -> String -> String
moduleTypeCheckError sourcePath phase err =
  "Module typecheck failed for "
    ++ sourcePath
    ++ " ("
    ++ phase
    ++ "):\n"
    ++ err

prepareInferenceDeclsForTypeInference ::
  Option ->
  Bool ->
  [Import] ->
  [ModuleInferenceDecl] ->
  ExceptT String IO [ModuleInferenceDecl]
prepareInferenceDeclsForTypeInference opts emitOutput imps inferenceDecls = do
  let verbose = emitOutput && optVerbose opts
      noDesugarCalls = optNoDesugarCalls opts
      noGenDispatch = optNoGenDispatch opts
      prettyInferenceDecls inferenceDumpDecls =
        pretty (CompUnit imps (moduleInferenceTopDecls inferenceDumpDecls))
      timeItNamed :: String -> IO a -> IO a
      timeItNamed
        | emitOutput = optTimeItNamed opts
        | otherwise = \_ action -> action

  -- contract field access desugaring
  let accessed = mapModuleInferenceTopDecls fieldDesugarTopDecls inferenceDecls
  liftIO $ when verbose $ do
    putStrLn "Contract field access desugaring:"
    putStrLn $ prettyInferenceDecls accessed

  -- contract dispatch generation
  dispatched <-
    liftIO $
      if noGenDispatch
        then pure accessed
        else timeItNamed "Contract dispatch generation" $ pure (mapModuleInferenceTopDecls contractDispatchTopDecls accessed)

  liftIO $ when (emitOutput && optDumpDispatch opts) $ do
    putStrLn "> Dispatch:"
    putStrLn $ prettyInferenceDecls dispatched

  -- Generic instance derivation (only for locally-defined data types)
  let localData = [dt | ModuleInferenceDecl ModuleLocalDecl (TDataDef dt) <- dispatched]
  derived <-
    ExceptT $
      runExceptT $
        traverseModuleInferenceTopDecls (ExceptT . pure . deriveGenericTopDecls localData) dispatched

  liftIO $ when verbose $ do
    putStrLn "> Generic instance derivation:"
    putStrLn $ prettyInferenceDecls derived

  -- SCC analysis
  connected <-
    ExceptT $
      timeItNamed "SCC           " $
        runExceptT $
          traverseModuleInferenceTopDecls (ExceptT . sccAnalysisTopDecls) derived

  liftIO $ when verbose $ do
    putStrLn "> SCC Analysis:"
    putStrLn $ prettyInferenceDecls connected

  -- Indirect call handling
  direct <-
    liftIO $
      if noDesugarCalls
        then pure connected
        else
          timeItNamed "Indirect Calls" $
            traverseModuleInferenceTopDecls (fmap fst . indirectCallTopDecls) connected

  liftIO $ when (emitOutput && (optVerbose opts || optDumpDF opts)) $ do
    putStrLn "> Indirect call desugaring:"
    putStrLn $ prettyInferenceDecls direct

  -- Pattern wildcard desugaring
  let noWild = mapModuleInferenceTopDecls replaceWildcardTopDecls direct
  liftIO $ when verbose $ do
    putStrLn "> Pattern wildcard desugaring:"
    putStrLn $ prettyInferenceDecls noWild

  -- Eliminate function type arguments
  let noFun = if noDesugarCalls then noWild else mapModuleInferenceTopDecls replaceFunParam noWild
  liftIO $ when verbose $ do
    putStrLn "> Eliminating argments with function types"
    putStrLn $ prettyInferenceDecls noFun

  -- Integer literal desugaring: wrap bare integer literals in fromInteger()
  let withFromInt = mapModuleInferenceTopDecls desugarIntLiterals noFun
  liftIO $ when verbose $ do
    putStrLn "> Integer literal desugaring:"
    putStrLn $ prettyInferenceDecls withFromInt

  pure withFromInt

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
