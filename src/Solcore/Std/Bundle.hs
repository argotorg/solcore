{-# LANGUAGE TemplateHaskell #-}

-- | The standard library, embedded as in-memory source so it can be mounted
-- into the virtual file system used by the in-memory compiler.
--
-- The import closure of @std@ is @std -> opcodes@ and @dispatch -> std,
-- opcodes@, so these three files form a self-contained set. Simple contracts
-- (see @test/examples/spec/00answer.solc@) use none of this and can be
-- compiled with dispatch generation disabled (@-g@).
module Solcore.Std.Bundle
  ( stdSources,
  )
where

import Solcore.Std.Embed (embedStringFile)

-- | Standard-library modules as @(file name, source)@ pairs. File names are
-- relative to the std root the loader mounts them under.
stdSources :: [(FilePath, String)]
stdSources =
  [ ("std.solc", $(embedStringFile "std/std.solc")),
    ("opcodes.solc", $(embedStringFile "std/opcodes.solc")),
    ("dispatch.solc", $(embedStringFile "std/dispatch.solc"))
  ]
