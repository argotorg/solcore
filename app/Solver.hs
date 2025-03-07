
import Solcore.Pipeline.SolverPipeline
import System.Environment
import System.Exit

main :: IO ()
main 
  = do
      args <- getArgs 
      case args of 
        [path] -> runForFile path 
        _ -> do 
          putStrLn "Usage: solver <path to solver input file>"
          exitFailure 
