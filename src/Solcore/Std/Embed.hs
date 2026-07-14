-- | Compile-time embedding of source files as string literals.
--
-- Used to bake the standard library into the binary so the in-memory / browser
-- compiler has no runtime file-system dependency.
module Solcore.Std.Embed
  ( embedStringFile,
  )
where

import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Syntax (addDependentFile, lift, runIO)

-- | Splice the contents of a file (read at compile time) as a 'String'
-- literal. The file is registered as a dependency so edits trigger a rebuild.
embedStringFile :: FilePath -> Q Exp
embedStringFile path = do
  addDependentFile path
  contents <- runIO (readFile path)
  lift contents
