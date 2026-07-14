{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Serialization for the typecheck cache (Phase 3).
--
-- All @Generic@/@Binary@ instances live here as standalone deriving, so the
-- @Syntax.*@ modules stay untouched. We use @binary@ (a GHC boot library, pure
-- Haskell, no dependency tail) so the same code cross-compiles to the JS
-- backend; the format is confined behind 'encodeCache' / 'decodeCache' and can
-- be swapped without touching the rest of the pipeline.
--
-- A cached module carries only what the assembly step actually reads back from a
-- non-entry module: its typed 'CompUnit' and the @typeTable@ of its 'TcEnv'
-- (see 'mergeCheckedModuleEnvs', which takes every other env field from the
-- never-cached entry module). The dropped 'CheckedModule' fields are restored as
-- loud error thunks: proven unread, so never forced, but failing clearly if that
-- assumption is ever violated.
module Solcore.Pipeline.TcCacheSerialize
  ( CachedModule (..),
    toCachedModule,
    fromCachedModule,
    encodeCache,
    decodeCache,
  )
where

import Data.Binary (Binary, get, put)
import Data.Binary.Get (getWord8, runGetOrFail)
import Data.Binary.Put (putWord8, runPut)
import Data.ByteString.Lazy qualified as BL
import Data.Map (Map)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Language.Yul (YLiteral (..), YulExp (..), YulStmt (..))
import Solcore.Frontend.Module.Identity
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Location (NodeLocation, generatedNode)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.TcEnv (TcEnv (..), TypeInfo (..), initTcEnv)
import Solcore.Frontend.TypeInference.TcModule (CheckedModule (..))
import Solcore.Pipeline.Options (Option)
import Solcore.Pipeline.TypecheckCache (TcCacheKey (..))

-- Names / identifiers -------------------------------------------------------

-- A 'Name' carries an optional source span that its 'Eq'/'Ord' ignore (they
-- compare only the name segments). Serialize just the segment structure and drop
-- the span, for the same toolchain-independence reason as 'NodeLocation' below;
-- reconstruct with the span-less 'Name'/'QualName' patterns.
instance Binary Name where
  put (Name s) = putWord8 0 >> put s
  put (QualName qualifier s) = putWord8 1 >> put qualifier >> put s
  get = do
    tag <- getWord8
    case tag of
      0 -> Name <$> get
      1 -> QualName <$> get <*> get
      _ -> fail ("Binary Name: unknown constructor tag " ++ show tag)

deriving stock instance Generic Id

deriving anyclass instance Binary Id

deriving stock instance Generic LibraryId

deriving anyclass instance Binary LibraryId

deriving stock instance Generic ModuleId

deriving anyclass instance Binary ModuleId

-- Types ---------------------------------------------------------------------
deriving stock instance Generic Tyvar

deriving anyclass instance Binary Tyvar

deriving stock instance Generic Ty

deriving anyclass instance Binary Ty

deriving stock instance Generic MetaTv

deriving anyclass instance Binary MetaTv

deriving stock instance Generic Pred

deriving anyclass instance Binary Pred

deriving stock instance Generic (Qual t)

deriving anyclass instance (Binary t) => Binary (Qual t)

deriving stock instance Generic Scheme

deriving anyclass instance Binary Scheme

deriving stock instance Generic TypeInfo

deriving anyclass instance Binary TypeInfo

-- Top level -----------------------------------------------------------------
deriving stock instance Generic (CompUnit a)

deriving anyclass instance (Binary a) => Binary (CompUnit a)

deriving stock instance Generic (TopDecl a)

deriving anyclass instance (Binary a) => Binary (TopDecl a)

deriving stock instance Generic (Contract a)

deriving anyclass instance (Binary a) => Binary (Contract a)

deriving stock instance Generic (ContractDecl a)

deriving anyclass instance (Binary a) => Binary (ContractDecl a)

deriving stock instance Generic (Field a)

deriving anyclass instance (Binary a) => Binary (Field a)

deriving stock instance Generic (Constructor a)

deriving anyclass instance (Binary a) => Binary (Constructor a)

deriving stock instance Generic (FunDef a)

deriving anyclass instance (Binary a) => Binary (FunDef a)

deriving stock instance Generic (Signature a)

deriving anyclass instance (Binary a) => Binary (Signature a)

deriving stock instance Generic (Class a)

deriving anyclass instance (Binary a) => Binary (Class a)

deriving stock instance Generic (Instance a)

deriving anyclass instance (Binary a) => Binary (Instance a)

deriving stock instance Generic DataTy

deriving anyclass instance Binary DataTy

deriving stock instance Generic Constr

deriving anyclass instance Binary Constr

deriving stock instance Generic TySym

deriving anyclass instance Binary TySym

deriving stock instance Generic TopDeclKey

deriving anyclass instance Binary TopDeclKey

-- Modules / imports / exports ----------------------------------------------
deriving stock instance Generic Import

deriving anyclass instance Binary Import

deriving stock instance Generic ModulePath

deriving anyclass instance Binary ModulePath

deriving stock instance Generic Export

deriving anyclass instance Binary Export

deriving stock instance Generic ExportSpec

deriving anyclass instance Binary ExportSpec

deriving stock instance Generic ExportSelector

deriving anyclass instance Binary ExportSelector

deriving stock instance Generic ExportSelectorEntry

deriving anyclass instance Binary ExportSelectorEntry

deriving stock instance Generic ConstructorSelector

deriving anyclass instance Binary ConstructorSelector

deriving stock instance Generic ItemSelector

deriving anyclass instance Binary ItemSelector

deriving stock instance Generic ItemSelectorEntry

deriving anyclass instance Binary ItemSelectorEntry

-- Pragmas -------------------------------------------------------------------
deriving stock instance Generic Pragma

deriving anyclass instance Binary Pragma

deriving stock instance Generic PragmaType

deriving anyclass instance Binary PragmaType

deriving stock instance Generic PragmaStatus

deriving anyclass instance Binary PragmaStatus

-- Statements / expressions / patterns --------------------------------------

-- A 'NodeLocation' is diagnostic metadata, not part of a module's typechecked
-- meaning (its 'Eq'/'Ord' are total), and its source span carries a
-- build-specific file path. Persisting spans would make the cache blob depend on
-- where std was compiled, breaking the content-addressed cache's toolchain
-- independence: the native gen-std-cache blob must be byte-identical to what the
-- browser would dump. Serialize every location as the generated (span-less) node
-- so cached ASTs round-trip deterministically; cached modules typecheck cleanly,
-- so the lost spans never surface in a diagnostic.
instance Binary NodeLocation where
  put _ = pure ()
  get = pure generatedNode

deriving stock instance Generic (Stmt a)

deriving anyclass instance (Binary a) => Binary (Stmt a)

deriving stock instance Generic (Param a)

deriving anyclass instance (Binary a) => Binary (Param a)

deriving stock instance Generic (Exp a)

deriving anyclass instance (Binary a) => Binary (Exp a)

deriving stock instance Generic (Pat a)

deriving anyclass instance (Binary a) => Binary (Pat a)

deriving stock instance Generic Literal

deriving anyclass instance Binary Literal

-- Inline Yul (reachable from the typed AST via @Asm YulBlock@) ---------------
deriving stock instance Generic YulStmt

deriving anyclass instance Binary YulStmt

deriving stock instance Generic YulExp

deriving anyclass instance Binary YulExp

deriving stock instance Generic YLiteral

deriving anyclass instance Binary YLiteral

-- Cache key -----------------------------------------------------------------
deriving stock instance Generic TcCacheKey

deriving anyclass instance Binary TcCacheKey

-- Payload -------------------------------------------------------------------

-- | The serialized form of a typechecked module: exactly what the assembly step
-- reads back from a non-entry module.
data CachedModule
  = CachedModule
  { cachedModuleId :: ModuleId,
    cachedTyped :: CompUnit Id,
    cachedTypeTable :: Map Name TypeInfo
  }
  deriving stock (Generic)
  deriving anyclass (Binary)

toCachedModule :: CheckedModule -> CachedModule
toCachedModule cm =
  CachedModule
    { cachedModuleId = checkedModuleId cm,
      cachedTyped = checkedModuleTyped cm,
      cachedTypeTable = typeTable (checkedModuleEnv cm)
    }

-- | Rebuild a 'CheckedModule' from its cached payload. The env is @initTcEnv@
-- with the cached @typeTable@ spliced in — the only env field read back from a
-- non-entry module. The one unread field is a loud error thunk.
fromCachedModule :: Option -> CachedModule -> CheckedModule
fromCachedModule opts cached =
  CheckedModule
    { checkedModuleId = cachedModuleId cached,
      checkedModuleTyped = cachedTyped cached,
      checkedModuleEnv = (initTcEnv opts) {typeTable = cachedTypeTable cached},
      checkedModuleInput = error "tc-cache: checkedModuleInput not restored from cache"
    }

-- | Magic number identifying a typecheck-cache blob ("STC" ++ 0x01).
tcCacheMagic :: Word32
tcCacheMagic = 0x53544301

-- | Format version. BUMP THIS whenever any serialized type changes shape (a
-- constructor or field added, removed, or reordered), so a dump written by an
-- incompatible build is rejected rather than misread. A rejected dump degrades
-- to a cache miss (recompute) — never a wrong result.
tcCacheFormatVersion :: Word32
tcCacheFormatVersion = 1

-- | Encode the keyed cache behind a magic + version header.
encodeCache :: Map TcCacheKey CachedModule -> BL.ByteString
encodeCache cache =
  runPut (put tcCacheMagic >> put tcCacheFormatVersion >> put cache)

-- | Decode a cache blob, returning 'Nothing' if the header is absent or does
-- not match (foreign format, wrong version, or corruption). The payload is only
-- parsed once the header matches, so a version mismatch can never crash while
-- decoding stale bytes.
decodeCache :: BL.ByteString -> Maybe (Map TcCacheKey CachedModule)
decodeCache blob =
  case runGetOrFail getChecked blob of
    Right (_, _, result) -> result
    Left _ -> Nothing
  where
    getChecked = do
      magic <- get
      version <- get
      if magic == tcCacheMagic && version == tcCacheFormatVersion
        then Just <$> get
        else pure Nothing
