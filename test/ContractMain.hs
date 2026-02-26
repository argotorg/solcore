module Main where

import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.Exit (die, exitWith)
import System.FilePath ((</>), takeDirectory)
import System.Process (rawSystem)

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  repoRoot <- findRepoRoot cwd
  setCurrentDirectory repoRoot

  putStrLn "Running contract tests through scripts/cabal-contract-tests.sh"
  exitCode <- rawSystem "bash" ["./scripts/cabal-contract-tests.sh"]
  exitWith exitCode

findRepoRoot :: FilePath -> IO FilePath
findRepoRoot dir = do
  hasCabalFile <- doesFileExist (dir </> "sol-core.cabal")
  if hasCabalFile
    then pure dir
    else do
      let parent = takeDirectory dir
      if parent == dir
        then die "Could not locate repository root containing sol-core.cabal."
        else findRepoRoot parent
