-- | In-memory compilation entry point.
--
-- This is the seam the web IDE talks to: source text in (from an editor
-- buffer), structured result out (compiler output or diagnostics), with no
-- file-system access. It reuses the whole existing pipeline via
-- 'compileGraph', differing from the CLI only in how the module graph is
-- obtained.
module Solcore.Api
  ( CompileResult (..),
    compileSolcore,
    defaultOptions,
  )
where

import Control.Monad.Except (runExceptT)
import Data.List (intercalate)
import Language.Hull qualified as Hull
import Language.Hull.ToYul.Assemble (objectToYul)
import Solcore.Frontend.Module.Loader (loadModuleGraphFromSource)
import Solcore.Frontend.Pretty.SolcorePretty (pretty)
import Solcore.Pipeline.Options (Option, emptyOption)
import Solcore.Pipeline.SolcorePipeline (compileGraph)

-- | Outcome of an in-memory compilation.
--
--   * 'compileOutput' — pretty-printed Hull objects (the frontend result).
--   * 'compileYul'    — generated Yul (the backend result), when the frontend
--     succeeded and Yul translation didn't fail.
--   * 'compileErrors' — diagnostics from whichever stage failed.
data CompileResult
  = CompileResult
  { compileOutput :: Maybe String,
    compileYul :: Maybe String,
    compileErrors :: [String]
  }
  deriving (Eq, Show)

-- | Baseline options for the in-memory path. The file-system fields of
-- 'Option' (roots, output dir) are unused here; the UI is expected to override
-- the boolean pass toggles (checkboxes) and the partial-evaluation fuel (an
-- input field) on top of this.
defaultOptions :: Option
defaultOptions = emptyOption "Main.solc"

-- | Compile a single solcore source module in memory. 'Option' is supplied by
-- the caller (the UI), so pass toggles and fuel are driven from the frontend.
compileSolcore :: Option -> String -> IO CompileResult
compileSolcore opts source = do
  graphResult <- loadModuleGraphFromSource source
  case graphResult of
    Left err -> pure (CompileResult Nothing Nothing [err])
    Right graph -> do
      compiled <- runExceptT (compileGraph opts graph)
      case compiled of
        Left err -> pure (CompileResult Nothing Nothing [err])
        Right objs -> do
          let hull = renderObjects objs
          yulResult <- objectsToYul objs
          pure $ case yulResult of
            Left err -> CompileResult (Just hull) Nothing [err]
            Right yul -> CompileResult (Just hull) (Just yul) []

renderObjects :: [Hull.Object] -> String
renderObjects = unlines . map pretty

-- | Translate all hull objects to Yul, concatenating them. Fails with the first
-- Hull/Yul type error encountered.
objectsToYul :: [Hull.Object] -> IO (Either String String)
objectsToYul objs = do
  results <- mapM objectToYul objs
  pure (intercalate "\n\n" <$> sequence results)
