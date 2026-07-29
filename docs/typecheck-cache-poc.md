# Typecheck cache — proof of concept

## Motivation

Every compile re-typechecks the whole module graph from scratch, including the
standard library. For the edit–recompile loop (and especially the browser IDE)
the std typecheck dominates wall-clock time even though std never changes. This
PoC shows we can **type-check each module once, persist the result to disk, and
reuse it** on later compiles whenever the module (and everything it depends on)
is unchanged — without affecting the compiler's output.

## The idea

Type-checking a module is a pure function of:

1. the module's own source,
2. the public interfaces of the modules it references, and
3. the compile flags that reach the type-checker.

So we give every module a **content-addressed Merkle key**:

```
key(M) = keccak( contentHash(M)
              ++ sorted [ key(R) | R <- references(M) ]
              ++ flagComponent(M) )
```

folded over the graph in dependency order. Two modules with equal keys are
guaranteed to type-check to the same result, so a cache indexed by key is sound.
Editing a module changes its key and, transitively, the key of every module that
references it — **and nothing else**. That gives precise invalidation: edit
`std.dispatch` and only `std.dispatch` + its dependents are rechecked; `std` and
`std.opcodes` are reused.

The type-checked result of each module is serialized to a **binary blob on
disk**, keyed by its Merkle key. On the next compile we recompute the keys
(cheap — parsing + hashing) and, for every key already on disk, load the checked
module instead of re-type-checking it.

## What's in this PR

| File | Role |
|------|------|
| `src/Solcore/Pipeline/TypecheckCache.hs` | Merkle cache keys; precise-invalidation helper (`transitiveDependents`) |
| `src/Solcore/Pipeline/TcCacheSerialize.hs` | `binary` serialization of a checked module; magic + version header |
| `src/Solcore/Pipeline/SolcorePipeline.hs` | `compileGraphWithCache` — reuse cached modules instead of re-checking |
| `src/Solcore/Util/Keccak.hs` | pure-Haskell keccak-256 used for the content hashes |
| `poc/Main.hs` | `tc-cache-poc` harness that demonstrates and checks all of the above |

### Cache keys (`TypecheckCache.hs`)

- `contentHash` is taken over the **parsed** compilation unit, so it's
  insensitive to comments/whitespace for free.
- Reference keys are folded in dependency order and sorted, so the key is
  independent of import order.
- `flagComponent` records only the flags that affect type-checking
  (`--no-desugar-calls`); `-g`/dispatch generation is folded in **only for
  modules that contain contracts**, so library keys stay stable as the UI
  toggles `-g`.

### Serialization (`TcCacheSerialize.hs`)

- A cached module carries only what the assembly step reads back from a
  non-entry module: its typed `CompUnit` and the `typeTable` of its environment.
  The other environment fields are restored as loud error thunks — proven
  unread, so never forced, but failing clearly if that assumption is violated.
- Every blob is prefixed with a **magic number + format version**. A dump
  written by an incompatible build (or a corrupted/foreign file) is rejected and
  degrades to a cache miss (recompute) — never a wrong result.
- We use `binary` (a GHC boot library, pure Haskell) rather than a JSON/aeson
  stack: compact, no dependency tail, and the format is confined behind
  `encodeCache` / `decodeCache` so it can be swapped without touching the rest.

### Pipeline seam (`SolcorePipeline.hs`)

`compileGraphWithCache opts graph cache` runs the normal pipeline but reuses any
module present in `cache` (keyed by `ModuleId`) instead of type-checking it, and
returns the full set of checked modules so a caller can seed the next run. The
existing `compile` path is unchanged (it calls this with an empty cache).

## Results

`cabal run tc-cache-poc -- test/examples/dispatch/counter.solc` demonstrates, on
a 4-module graph (`std`, `std.opcodes`, `std.dispatch`, `counter`):

- **Merkle keys** printed per module.
- **Invalidation property** (checked as an assertion): editing `std.opcodes`
  rechecks all 4; `std` → 3; `std.dispatch` → 2; `counter` → 1 — exactly the
  transitive-dependent set.
- **Determinism**: keys are stable across a reload of the same sources.
- **On-disk round-trip**: the cold-checked modules are written to
  `solcore-tc-cache.bin`, read back, decoded, and used to compile again.
- **Format guard**: a blob with a corrupted header is rejected.
- **Correctness**: the in-process warm/hit runs and the on-disk run all produce
  **byte-identical hull** to the cold run.

Representative timing (type-check phase is the part the cache elides):

```
objects: 1 | cold 3.74s | warm 1.05s | hit 0.20s | disk 0.19s
```

i.e. reusing the std subset from disk turns a ~3.7s cold compile into ~0.2s,
reproducing the cold output exactly. The on-disk dump for this 4-module graph is
~3.3 MB (see the note on location data below).

## Scope / limitations (deliberate, for review)

- **Type-check only.** Specialization is whole-program (monomorphization across
  the entire program) and is *not* cached — it re-runs every compile. The cache
  elides the per-module type-check, which is the dominant cost for std.
- **No cyclic import groups yet.** Modules in an import cycle share an SCC;
  SCC-group keying is not implemented, and the PoC fails loudly if it hits one.
- **Harness, not CLI integration.** The disk cache is exercised by `tc-cache-poc`
  to validate the concept; wiring it into the `sol-core` CLI (cache directory,
  eviction, invalidation on flag change) is the natural next step.
- **Native only.** A browser/session variant (in-memory + IndexedDB) exists on a
  separate branch and is out of scope here.
- **Blob size.** The typed AST now carries a source location on every node, so
  the serialized modules are larger than they need to be (~3.3 MB for the 4
  above). Since node equality already ignores locations, a real integration
  would likely drop or normalize them before serializing.

## How to run

```bash
cabal run tc-cache-poc -- test/examples/dispatch/counter.solc
```
