module Solcore.Pipeline.SolcorePipeline where

import Control.Monad

import qualified Data.Map as Map

import Options.Applicative

import Solcore.Desugarer.MatchCompiler
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

import System.Exit

-- main compiler driver function

pipeline :: IO ()
pipeline = do
  opts <- argumentsParser
  let verbose = optVerbose opts
  content <- readFile (fileName opts)
  t' <- runParser content 
  withErr t' $ \ ast@(CompUnit imps ds) -> do
    when verbose $ do 
      putStrLn "AST after name resolution"
      putStrLn $ pretty ast 
    r2 <- sccAnalysis ast
    withErr r2 $ \ ast' -> do
      when verbose $ do 
        putStrLn "SCC Analysis:"
        putStrLn $ pretty ast'
      r5 <- typeInfer1 ast'
      withErr r5 $ \ (c', env) -> do
        let warns = warnings env
            logsInfo = logs env
            tyctx = ctx env 
            (ts, cmap) = generated env 
        when (verbose && (not $ null logsInfo)) $ do  
          putStrLn "> Type inference logs:"
          mapM_ putStrLn (reverse $ logsInfo)
        let ast0 = moveData (addGenerated ast' ts cmap)
        r6 <- sccAnalysis ast0
        withErr r6 $ \ ast2 -> do 
          when verbose $ do 
            putStrLn "> Desugared code - step 1"
            putStrLn $ pretty ast2
          r7 <- typeInfer2 env ast2 
          withErr r7 $ \ (c1, env1) -> do 
            let warns1 = warnings env1 
            let logsInfo = logs env1 
            when (not $ null warns) $ do 
              putStrLn "> Type inference warnings:"
              mapM_ putStrLn (reverse $ warns) 
            when (verbose && (not $ null logsInfo)) $ do  
              putStrLn "> Type inference logs:"
              mapM_ putStrLn (reverse $ logsInfo)
            when verbose $ do
              putStrLn "> Annotated AST:"
              putStrLn $ pretty c1
            r8 <- matchCompiler c1
            withErr r8 $ \ res -> do
              when (verbose || optDumpDS opts) do
                putStrLn "Match compilation result:"
                putStrLn (pretty res)
              unless (optNoSpec opts) do
                r9 <- specialiseCompUnit res (optDebugSpec opts) env
                when (optDumpSpec opts) do
                  putStrLn "Specialised contract:"
                  putStrLn (pretty res)
                r10 <- emitCore (optDebugCore opts) env res
                when (optDumpCore opts) do
                  putStrLn "Core contract(s):"
                  forM_ r10 (putStrLn . pretty)

runParser :: String -> IO (Either String (CompUnit Name))
runParser content = do 
  let r1 = runAlex content parser 
  case r1 of 
    Left err -> pure $ Left err 
    Right t -> do 
      buildAST t

withErr :: Either String a -> (a -> IO ()) -> IO ()
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
                Map.Map Name [ContractDecl Name] -> 
                CompUnit Name 
addGenerated (CompUnit imps ds) ts m 
  = CompUnit imps (ds' ++ ts) 
    where 
      ds' = foldr step [] ds 
      step (TContr (Contract n vs cs)) ac 
        = (TContr (Contract n vs (cs ++ Map.findWithDefault [] n m))) : ac 
      step d ac = d : ac 

-- parsing command line arguments

data Option
  = Option
    { fileName :: FilePath
    , optNoSpec :: !Bool
    -- Options controlling printing
    , optVerbose :: !Bool
    , optDumpDS :: !Bool
    , optDumpDF :: !Bool
    , optDumpSpec :: !Bool
    , optDumpCore :: !Bool
    -- Options controlling diagnostic output
    , optDebugSpec :: !Bool
    , optDebugCore :: !Bool
    } deriving (Eq, Show)

options :: Parser Option
options
  = Option <$> strOption (
                  long "file"
               <> short 'f'
               <> metavar "FILE"
               <> help "Input file name")
           <*> switch ( long "no-specialise"
               <> short 'n'
               <> help "Skip specialisation and core emission phases")
           -- Options controlling printing
           <*> switch ( long "verbose"
               <> short 'v'
               <> help "Verbose output")
           <*> switch ( long "dump-ds"
               <> help "Dump desugared contract")
           <*> switch ( long "dump-df"
               <> help "Dump defunctionalised contract")
           <*> switch ( long "dump-spec"
               <> help "Dump specialised contract")
           <*> switch ( long "dump-core"
               <> help "Dump low-level core")
           -- Options controlling diagnostic output
           <*> switch ( long "debug-spec"
               <> help "Debug specialisation")
           <*> switch ( long "debug-core"
               <> help "Debug core emission")
argumentsParser :: IO Option
argumentsParser = do
  let opts = info (options <**> helper)
                  (fullDesc <>
                   header "Solcore - solidity core language")
  opt <- execParser opts
  return opt
