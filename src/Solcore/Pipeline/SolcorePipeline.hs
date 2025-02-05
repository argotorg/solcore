module Solcore.Pipeline.SolcorePipeline where

import Control.Monad

import qualified Data.Map as Map

import Solcore.Desugarer.IndirectCall (indirectCall)
import Solcore.Desugarer.LambdaLifting (lambdaLifting)
import Solcore.Desugarer.MatchCompiler
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
import System.Exit

-- main compiler driver function

pipeline :: IO ()
pipeline = do
  opts <- argumentsParser
  let verbose = optVerbose opts
  let noDesugarCalls = optNoDesugarCalls opts
  content <- readFile (fileName opts)
  t' <- runParser content
  withErr t' $ \ ast@(CompUnit imps ds) -> do
    when verbose $ do
      putStrLn "> AST after name resolution"
      putStrLn $ pretty ast
    r5 <- if noDesugarCalls
      then do
        r2 <- sccAnalysis ast
        withErr r2 $ \ ast' -> do
          when verbose $ do
            putStrLn "> SCC Analysis:"
            putStrLn $ pretty ast'
          typeInfer opts Map.empty ast'
      else do
        (ast0, mdt) <- uniqueTypeGen ast
        when verbose $ do
          putStrLn "> Unique type generation"
          putStrLn $ pretty ast0
        r2 <- sccAnalysis ast0
        withErr r2 $ \ ast' -> do
          when verbose $ do
            putStrLn "> SCC Analysis:"
            putStrLn $ pretty ast'
          ast3 <- indirectCall mdt ast'
          when verbose $ do
            putStrLn "> Indirect call desugaring:"
            putStrLn $ pretty ast3
          typeInfer opts mdt ast3
    withErr r5 $ \ (c', env) -> do
        let warns = warnings env
            logsInfo = logs env
            tyctx = ctx env
            ts = generated env
        when verbose $ do
          putStrLn "> Type inference logs:"
          mapM_ putStrLn (reverse $ logsInfo)
          putStrLn "> Elaborated tree:"
          putStrLn $ pretty c'
        r8 <- matchCompiler c'
        withErr r8 $ \ res -> do
          when (verbose || optDumpDS opts) do
            putStrLn "> Match compilation result:"
            putStrLn (pretty res)
          unless (optNoSpec opts) do
            r9 <- specialiseCompUnit res (optDebugSpec opts) env
            when (optDumpSpec opts) do
              putStrLn "> Specialised contract:"
              putStrLn (pretty r9)
            r10 <- emitCore (optDebugCore opts) env r9
            when (optDumpCore opts) do
              putStrLn "> Core contract(s):"
              forM_ r10 (putStrLn . pretty)

runParser :: String -> IO (Either String (CompUnit Name))
runParser content = do
  let r1 = runAlex content parser
  case r1 of
    Left err -> pure $ Left err
    Right t -> do
      buildAST t

withErr :: Either String a -> (a -> IO b) -> IO b
withErr r f
  = either err f r
    where
      err s = do
                putStrLn s
                exitWith (ExitFailure 1)

-- add declarations generated in the previous step
-- and moving data types inside contracts to the
-- global scope.

moveData :: CompUnit Name -> CompUnit Name
moveData (CompUnit imps decls)
  = CompUnit imps (foldr step [] decls)
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
