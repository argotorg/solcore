# Architecture

This document describes Solcore's high-level compilation-pipeline architecture: the flow from source to Hull/Yul, the key modules involved at each stage, the type system, specialization/Mast, comptime evaluation, diagnostics, and the module system. It is referenced from [`CLAUDE.md`](../CLAUDE.md) and kept in sync with the code in `src/Solcore/Pipeline/SolcorePipeline.hs`.

## Compilation Pipeline Flow

```
Sources (.solc, multi-module) → Module Loader → Parser → Name Resolution
   → Early Desugaring (untyped) → Type Checker (per module) → Late Desugaring (typed)
   → Specialization → Mast → Comptime Evaluation / Dead-Code Elimination → Comptime Check
   → Hull Emission → Hull IR
                                                                              ↑
                                                       Hull → Yul Translation (`yule`)
                                          Frontend + middle end (sol-core)      Backend (yule)
```

`compileWithDiagnostics` in `src/Solcore/Pipeline/SolcorePipeline.hs` is the top-level driver. It:

1. Loads the **module graph** for the entry file (`Solcore.Frontend.Module.Loader`), resolving
   imports against the main library root, the std library root, and any registered external
   libraries (`--lib NAME=DIR`).
2. Validates and name-resolves each module against only its own direct imports.
3. Type-checks each module (`Solcore.Frontend.TypeInference.TcModule` /
   `Solcore.Frontend.TypeInference.TcContract`), threading a per-module pipeline of early
   desugaring passes before inference.
4. Assembles the checked modules into a single typed compilation unit.
5. Runs a SAIL-level ("early") comptime check, then late desugaring, specialization, comptime
   (partial) evaluation, dead-code elimination, a Mast-level comptime check, and finally Hull
   emission.
6. On failure at any stage, produces rich `Diagnostic`s (`Solcore.Diagnostics`) rendered with
   source spans, labels, notes and help text; on success, writes one `outputN.hull` file per
   compiled contract/object.

**Per-module pipeline (untyped, run once per module before type inference)**, driven by
`prepareInferenceDeclsForTypeInference`:
1. **Field Access Desugaring** (`Solcore.Desugarer.FieldAccess`) — desugar contract field access
   syntax
2. **ABI emission** (`Solcore.Desugarer.ContractDispatch.writeContractAbis`, gated by `--abi`)
3. **Contract Dispatch Generation** (`Solcore.Desugarer.ContractDispatch`) — generate method
   dispatch code for contracts
4. **Generic instance derivation** (`Solcore.Desugarer.DeriveGeneric`) — derive `Generic` instances
   for data types
5. **Class instance derivation** (`Solcore.Desugarer.DeriveClasses`) — expand `deriving (...)`
   clauses into forwarding instances via `Generic`
5b. **Struct field-projection generation** (`Solcore.Desugarer.StructProjection`) — a `struct` is a
   single-constructor product (`data Foo = Foo(T1, …, Tn)`) whose constructor also carries the
   field names. This pass emits one positional projection function per field; dot-notation access
   `s.x` is then rewritten to a projection call during type checking (`TcStmt`, using the same
   `fieldProjName` mangling). Because a struct is just a tagless product, it inherits the tuple ABI
   encoding and the SoA/`fst`/`snd` backend lowering unchanged, so no backend work is needed —
   struct ABI encode/decode is exactly Solidity's tuple wire format.
6. **SCC Analysis** (`Solcore.Frontend.TypeInference.SccAnalysis`) — analyze strongly connected
   components for mutual recursion
7. **Indirect Call Handling** (`Solcore.Desugarer.IndirectCall`) — defunctionalization (eliminate
   higher-order functions)
8. **Wildcard Replacement** (`Solcore.Desugarer.ReplaceWildcard`) — replace pattern wildcards with
   fresh variables
9. **Function Type Argument Elimination** (`Solcore.Desugarer.ReplaceFunTypeArgs`) — remove
   function-typed parameters
10. **Integer Literal Desugaring** (`Solcore.Desugarer.IntLiteralDesugar`) — wrap bare integer
    literals in `fromInteger(...)`
11. **String Literal Desugaring** (`Solcore.Desugarer.StrLiteralDesugar`) — wrap bare string
    literals in `fromString(...)`

**Type Checking** (per module, then assembled):
- **Type Inference** (`Solcore.Frontend.TypeInference.TcModule` / `TcContract`) — constraint-based
  bidirectional type checking → typed AST (`CompUnit Id`)
- A frontend/"early" comptime check (`Solcore.Frontend.ComptimeCheck`) runs on the assembled typed
  AST before late desugaring

**Late Desugaring & Lowering (typed AST → Hull)**:
1. **If/Bool Desugaring** (`Solcore.Desugarer.IfDesugarer`) — lower if-expressions to pattern
   matching on sum types
2. **Match Compilation** (`Solcore.Desugarer.DecisionTreeCompiler`) — compile complex patterns to
   simple decision trees (Augustsson's algorithm); also produces non-exhaustiveness/redundancy
   warnings
3. **Specialization** (`Solcore.Backend.Specialise`) — monomorphize polymorphic/overloaded code
   into **Mast** (`Solcore.Backend.Mast`), a monomorphic AST with no type variables or meta
   variables
4. **Comptime evaluation** (`Solcore.Backend.MastEval`) — a partial evaluator over Mast: folds
   arithmetic/assembly on literals, propagates known values, inlines simple pure functions,
   evaluates comptime-only `integer`/`string` values, fuel-bounded (`--pe-fuel N`)
5. **Dead-code elimination** (`Solcore.Backend.MastEval.eliminateDeadCode`) — removes functions
   unreachable from the contract's entry points
6. **Backend comptime check** (`Solcore.Backend.ComptimeCheck`) — verifies `comptime`
   annotations (parameters, `let` bindings, return types) are consistent with what the partial
   evaluator could actually resolve
7. **Hull Emission** (`Solcore.Backend.EmitHull`) — translate Mast into `Language.Hull.Object`
   values (one Hull object per emitted contract), a stack-machine-like IR with explicit sums,
   pairs, pattern matches and inline Yul assembly blocks

**Hull → Yul Translation (`yule` binary / `Language.Hull.ToYul.*`)**:
- Optional Hull type checking (`Language.Hull.TypeCheck`, skippable with `--no-typecheck`)
- Optional sum-representation compression (`Language.Hull.Compress`)
- Translation to Yul (`Language.Hull.ToYul.Translate`, using `Language.Hull.ToYul.TM` as the
  translation monad and `Language.Hull.ToYul.Locus` for stack/memory location tracking)
- Wrapping into a deployable Yul object, with create/runtime code
  (`Language.Hull.ToYul.Assemble`), unless `--nodeploy`/`--runonce` is passed

**Key Insight**: Early (untyped) desugaring simplifies contract-specific syntax, derives generated
instances, and eliminates higher-order functions BEFORE type checking, so the type checker works on
a simpler, more uniform AST. Late (typed) desugaring/lowering handles constructs that need type
information: if/pattern compilation, monomorphization, comptime evaluation, and Hull emission.

## Key Modules

**Pipeline Orchestration**:
- `src/Solcore/Pipeline/SolcorePipeline.hs` - Main compilation pipeline and diagnostic plumbing
- `src/Solcore/Pipeline/Options.hs` - CLI option parsing (`sol-core` flags)

**Module System & Diagnostics**:
- `src/Solcore/Frontend/Module/Loader.hs` - Module graph construction, import resolution, per-module
  validation surfaces
- `src/Solcore/Frontend/Module/Identity.hs` - Module identity (library + logical module path) vs.
  physical file path
- `src/Solcore/Diagnostics.hs` - Diagnostic codes, source spans/labels, diagnostic rendering
  (plain/JSON, color, unicode), `CompilerError`
- `doc/module-system.md` - Specification of import syntax, module identity, and library roots

**Phase 1: Parsing & Early Desugaring (Untyped)**:
- `src/Solcore/Frontend/Parser/` - Lexer and parser
- `src/Solcore/Frontend/Syntax/` - AST definitions
- `src/Solcore/Desugarer/FieldAccess.hs` - Field access desugaring
- `src/Solcore/Desugarer/ContractDispatch.hs` - Contract method dispatch generation, ABI emission
- `src/Solcore/Desugarer/DeriveGeneric.hs` - `Generic` instance derivation
- `src/Solcore/Desugarer/DeriveClasses.hs` - `deriving (...)` clause expansion
- `src/Solcore/Frontend/TypeInference/SccAnalysis.hs` - Dependency analysis
- `src/Solcore/Desugarer/IndirectCall.hs` - Defunctionalization (remove higher-order functions)
- `src/Solcore/Desugarer/ReplaceWildcard.hs` - Wildcard replacement
- `src/Solcore/Desugarer/ReplaceFunTypeArgs.hs` - Function type argument elimination
- `src/Solcore/Desugarer/IntLiteralDesugar.hs` - Integer literal desugaring (`fromInteger`)
- `src/Solcore/Desugarer/StrLiteralDesugar.hs` - String literal desugaring (`fromString`)
- `src/Solcore/Desugarer/UniqueTypeGen.hs` - Fresh/unique type synthesis helper

**Phase 2: Type Checking**:
- `src/Solcore/Frontend/TypeInference/TcModule.hs` - Per-module type inference entry points
- `src/Solcore/Frontend/TypeInference/TcContract.hs` - Type checking orchestration
- `src/Solcore/Frontend/TypeInference/TcMonad.hs` - Type checker monad
- `src/Solcore/Frontend/TypeInference/TcEnv.hs` - Type environment
- `src/Solcore/Frontend/TypeInference/TcUnify.hs` - Unification algorithm
- `src/Solcore/Frontend/TypeInference/TcSat.hs` / `TcSimplify.hs` / `TcResolution.hs` - Type class
  constraint solving/simplification/instance resolution
- `src/Solcore/Frontend/ComptimeCheck.hs` - Early (SAIL-level) comptime verification

**Phase 3: Late Desugaring & Lowering (Typed → Mast → Hull)**:
- `src/Solcore/Desugarer/IfDesugarer.hs` - If-expression desugaring (post-typecheck)
- `src/Solcore/Desugarer/DecisionTreeCompiler.hs` - Pattern matching compilation (Augustsson's
  algorithm)
- `src/Solcore/Backend/Specialise.hs` - Monomorphization of polymorphic/overloaded code
- `src/Solcore/Backend/Mast.hs` - Monomorphic AST (Mast) definition — the specializer's output
- `src/Solcore/Backend/MastEval.hs` - Partial evaluator ("comptime eval") and dead code elimination
  over Mast
- `src/Solcore/Backend/ComptimeCheck.hs` - Mast-level comptime annotation verification
- `src/Solcore/Backend/EmitHull.hs` - Translation from Mast to Hull IR
- `src/Language/Hull.hs` / `src/Language/Hull/Types.hs` - Hull IR definition

**Phase 4: Yul Backend (`yule` binary, backed by the library)**:
- `yule/Main.hs` - Yule binary entry point (thin CLI wrapper)
- `src/Language/Hull/Parser.hs` - Parser for `.hull` files
- `src/Language/Hull/TypeCheck.hs` / `TcEnv.hs` / `TcMonad.hs` - Hull type checking
- `src/Language/Hull/Compress.hs` - Sum-representation compression pass
- `src/Language/Hull/ToYul/Translate.hs` - Hull → Yul translation
- `src/Language/Hull/ToYul/TM.hs` - Translation monad
- `src/Language/Hull/ToYul/Locus.hs` - Location abstraction (stack/memory tracking)
- `src/Language/Hull/ToYul/Assemble.hs` - Wraps translated code into a deployable Yul object
- `src/Language/Yul.hs`, `Language/Yul/Parser.hs`, `Language/Yul/Builtins.hs` - Yul AST, parser, and
  builtin definitions

## Type System

The type system implements **HM(X)** with:
- **Parametric polymorphism** (generics with type variables)
- **Type classes** (Haskell-style with multi-parameter type classes)
- **Constraint-based type inference** (bidirectional checking)
- **Instance resolution** with overlapping instance checks (legacy or tabled resolution mode,
  selectable via `Solcore.Pipeline.Options.TypeClassResolutionMode`)

Type checking uses:
- **Inference mode**: Generate constraints from expressions (bottom-up)
- **Checking mode**: Check against expected types (top-down)
- **Constraint solving**: Unification + instance resolution + recursive constraint reduction

## Specialization (Monomorphization) and Mast

**Critical phase** that eliminates all polymorphism before code generation, producing **Mast**
(`Solcore.Backend.Mast`), a monomorphic AST with no type variables or meta variables:

1. Build resolution table: `(function name, concrete type) → specialized definition`
2. Analyze all call sites to find instantiation types
3. Create specialized versions with unique names (e.g., `map$word`, `map$bool`)
4. Resolve type class instances to concrete implementations
5. Recursively specialize all called functions

**Why necessary**:
- Yul and EVM have no polymorphism/generics
- All types must be concrete for memory layout
- Type class dispatch resolved statically (no virtual dispatch)
- Whole-program compilation required (must see all call sites)

## Comptime Evaluation, Dead-Code Elimination, and Comptime Checking

After specialization, Mast goes through a compile-time evaluation stage before Hull emission:

- **`Solcore.Backend.MastEval`** is a fuel-bounded partial evaluator (`--pe-fuel N`, default
  `defaultFuel`) that interprets supported Yul arithmetic in assembly blocks, folds calls like
  `subWord`/`gtWord`/`bxorWord`/`bandWord`/`borWord`/`bnotWord`/`eqWord` and string builtins
  (`concatLit`, `strlenLit`, `keccakLit`, `keccakWordLit`) on literal arguments, propagates known
  variable values, and inlines simple pure functions. It also eliminates the comptime-only
  `integer` and `string` types, which have no runtime representation and must be fully folded away
  before Hull emission.
- `eliminateDeadCode` (same module) then removes functions unreachable from the contract's
  entry points (`start`/`main`).
- **`Solcore.Backend.ComptimeCheck`** verifies that `comptime`-annotated parameters, `let`
  bindings, and return types are actually comptime after evaluation, reporting the first violation.

## Data Type Encoding

**Sum types** → Nested binary sums:
- `Either A (Either B C)` becomes `inl a | inr (inl b | inr c)`

**Product types** → Nested pairs:
- `(A, B, C)` becomes `(A, (B, C))`

This uniform encoding is used *internally* from Mast through Hull to Yul: a sum
value is a one-word discriminant followed by the (union-sized) branch payload, and
the discriminant is the positional `inl`/`inr` (`0`/`1`) chain of the nested binary
sums.

**ABI wire format of a sum** (distinct from the internal encoding). When a
multi-constructor ADT crosses the ABI boundary (a public method parameter/return,
via `std.ABIGeneric`), its discriminant is *not* the positional `0`/`1` chain but a
single `bytes32` **variant tag**: `keccak256("Name(argSigs...)")`, where `Name` is
the constructor name and `argSigs` are the field types rendered with the same
`SigString` convention (`std.dispatch`) used for method selectors — a full 32-byte
hash rather than the 4-byte selector truncation. The surrounding layout is
otherwise unchanged (static sum: inline `[tag][branch]`; dynamic sum: an offset
word to an inline `[tag][branch]` body in the tail), since a tag is still one word.
The variant name only survives to the point where `Solcore.Desugarer.DeriveGeneric`
emits each type's `ABIEncode`/`ABIDecode` instance, so those concrete instances (not
the anonymous structural `sum(f,g)` bridge) carry the tag; the helpers
`variantTag` / `encodeVariant` / `abiSumReader` live in `std.ABIGeneric`. A
single-constructor ADT is a product/struct with no discriminant and carries no tag.

## Diagnostics

`Solcore.Diagnostics` provides a structured diagnostic model used across parsing, name resolution,
module validation, and type checking:
- `Diagnostic`s carry a `Severity`, an optional `DiagnosticCode` (e.g. `SC0101` unknown name,
  `SC0201` type mismatch, `SC0225`/`SC0227`/`SC0228` duplicate definitions, `SC0301`/`SC0302`
  match-compiler warnings), labeled `SourceSpan`s (primary/secondary), notes, and help text.
- `SolcorePipeline` enriches diagnostics after the fact by searching source text/tokens for terms
  implicated by the error message, to attach precise labels even when the originating pass didn't
  compute a span directly.
- Rendering (`renderDiagnostics`) supports plain-text or JSON output, configurable color and
  unicode usage, and terminal width, controlled via CLI flags in `Solcore.Pipeline.Options`.
- Warning policy (`--warnings=default|always|never|deny`) controls whether match-compiler
  exhaustiveness/redundancy warnings are shown or promoted to errors.

## Module System

Solcore compiles multi-file programs via a small logical module system
(`Solcore.Frontend.Module.Loader`, `Solcore.Frontend.Module.Identity`; specified in
`doc/module-system.md`):
- Module identity is `(library, logical module path)`, independent of the physical file path;
  `foo.bar` maps to `foo/bar.solc`.
- Libraries are: the main library (`--root`, default `.`), the std library (`--include`, default
  `std`), and named external libraries (`--lib NAME=DIR`, referenced as `import @NAME.path;`).
- Import forms: `import M;`, `import M as A;`, `import M.{X, Y};`, `import M.{X as Z};`,
  `import M.{*};`, `import M.{*} hiding {X};`, plus `lib.`- and `@ext.`-qualified paths.
- Each module is name-resolved against only its own direct imports and then type-checked
  individually before all checked modules are assembled into one compilation unit.

## Common Patterns

**Monad Transformers**:
- `TcM` (`Solcore.Frontend.TypeInference.TcMonad`) for type checking
- `SM` (`Solcore.Backend.Specialise`) for specialization
- `EM` (`Solcore.Backend.EmitHull`) for Hull emission
- `EvalM` (`Solcore.Backend.MastEval`) for partial evaluation
- `TM` (`Language.Hull.ToYul.TM`) for Hull → Yul translation

**Generic Traversals**: Uses Scrap Your Boilerplate (`Data.Generics`):
```haskell
everywhere (mkT transform)  -- Apply transformation everywhere
everything (<>) (mkQ mempty collector)  -- Collect values
```

**Fresh Name Generation**: Thread-safe unique names via `NameSupply`:
```haskell
freshName :: TcM Name
freshTyVar :: TcM Ty
```
