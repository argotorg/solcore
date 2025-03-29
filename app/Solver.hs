import Solcore.Pipeline.SolverPipeline
import System.Environment
import System.Exit

main :: IO ()
main
  = do
      args <- getArgs
      case args of
        [path] -> do
          b <- runForFile path
          if b then pure () else exitFailure
        _ -> do 
          putStrLn "Usage: solver <path to solver input file>"
          exitFailure

