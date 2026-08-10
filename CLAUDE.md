# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Solcore is an experimental implementation of Solidity's new type system. It's a prototype compiler
that produces executable EVM code. The compiler implements a sophisticated type system with
parametric polymorphism (generics) and type classes (similar to Haskell), which are compiled down
to monomorphic code through specialization. Specialized code is lowered to **Mast** (a monomorphic,
first-order abstract syntax tree with no type variables), optionally partially evaluated at compile
time, and then emitted as **Hull** — a small stack/sum/product-oriented IR that is close to Yul but
still typed. Hull is translated to Yul by a separate stage, and from there external tools (`solc`,
`geth`/`hevm`) are relied upon to go from Yul to EVM bytecode.

The frontend also implements a small multi-file **module system** (imports, namespaces, a std
library, and named external libraries) and a **structured diagnostics** subsystem (error codes,
labeled source spans, rendered multi-line reports) used throughout parsing, name resolution, and
type checking.

**Important**: This is a research prototype, not production-ready. It contains bugs and is not
optimized for UX.

## Build & Development Commands

### Setup
```bash
# Enter development shell with all dependencies (recommended)
nix develop

# If nix flakes give errors, add to ~/.config/nix/nix.conf:
# experimental-features = nix-command flakes
```

The development shell includes:
- Haskell tools: GHC 9.10, cabal, HLS
- Solidity tools: solc, foundry-bin, hevm
- C++ tools: cmake, boost (for testrunner)
- Utilities: jq, go-ethereum, goevmlab

### Build
```bash
# Build the project
cabal build

# Build with nix (runs full CI pipeline locally)
nix build

# Enter REPL for interactive development
cabal repl
```

### Testing
```bash
# Run all tests
cabal test

# Run the main unit/spec test suite only
cabal test sol-core-tests

# Run specific test - the test suite uses tasty, individual tests can be filtered
cabal test --test-options="-p 'pattern'"

# Run the contract integration test suite via cabal (auto-builds testrunner,
# auto-detects evmone; fails if deps are missing unless
# SOLCORE_CONTRACT_TESTS_ALLOW_SKIP=1 is set)
cabal test sol-core-contract-test

# Build C++ testrunner directly (required for contest integration tests)
cmake -S . -B build
cmake --build build --target testrunner
# Creates: build/test/testrunner/testrunner

# Run contest integration tests
export testrunner_exe=build/test/testrunner/testrunner
bash run_contests.sh

# Or run contest tests via Nix (builds everything and runs tests automatically)
nix flake check
```

### Compilation Pipeline

The compiler is split into **two separate binaries**, both built from the same `sol-core` library:

1. **sol-core**: Parses, resolves modules/imports, typechecks, desugars, specializes
   (monomorphizes), partially evaluates, and lowers to Hull IR
2. **yule**: Translates Hull IR to Yul (the Hull→Yul translator itself lives in the library, under
   `Language.Hull.ToYul.*`, so it is reusable from tests and other tools; `yule` is a thin CLI
   wrapper around it)

```bash
# Compile .solc source to .hull IR (one file per emitted contract:
# output1.hull, output2.hull, ...)
cabal run sol-core -- -f <input.solc>

# Translate a .hull file to .yul
cabal run yule -- output1.hull -o output.yul

# Optional: skip deployment code generation
cabal run yule -- output1.hull -o output.yul --nodeploy
```

### Running Contracts

Use `runsol.sh` for the full pipeline (sol-core → yule → solc → geth):

```bash
# Basic execution
./runsol.sh <file.solc>

# With function call
./runsol.sh <file.solc> --runtime-calldata "transfer(address,uint256)" "0x123..." "100"

# With raw calldata
./runsol.sh <file.solc> --runtime-raw-calldata "0xabcd..."

# Skip deployment (run runtime code directly)
./runsol.sh <file.solc> --create false

# Debug with interactive trace viewer
./runsol.sh <file.solc> --debug-runtime
./runsol.sh <file.solc> --debug-create

# Pass value (in wei)
./runsol.sh <file.solc> --runtime-callvalue 1000000000
```

## High-Level Architecture

See [`doc/architecture.md`](doc/architecture.md) for the full description of the compilation pipeline flow, key modules per phase, the type system, specialization/Mast, comptime evaluation, Hull IR, diagnostics, the module system, and common patterns.

## Test Organization

Tests are organized in `test/`:

- `test/examples/spec/` - Specification test cases (core language features)
- `test/examples/cases/` - General test cases
- `test/examples/dispatch/` - Contract method dispatch tests (also drive the C++ testrunner via
  JSON test specs)
- `test/examples/pragmas/` - Pragma-related tests (bounds checking, coverage, etc.)
- `test/examples/comptime/` - Comptime evaluation/annotation tests
- `test/examples/hull/`, `test/examples/core/`, `test/examples/yule/` - Hull/Yul-level test inputs
- `test/examples/assembly/`, `test/examples/opcodes/`, `test/examples/invokable/`,
  `test/examples/attic/` - Additional targeted test cases
- `test/imports/` - Module/namespace import tests (large suite covering aliasing, hiding,
  re-exports, external libraries, ambiguity, cycles, etc.)
- `test/diagnostics/` - Fixtures for diagnostic rendering tests
- `test/solver/` - Constraint-solver test inputs

Test framework: **Tasty** with HUnit assertions. Test suites (see `sol-core.cabal`):
- `sol-core-tests` — the main suite (`test/Main.hs`), covering `Cases`, `ContractAbiTests`,
  `DiagnosticCliTests`, `DiagnosticTests`, `HullCases`, `LocationTests`, `MatchCompilerTests`,
  `ModuleTypeCheckTests`, `SpecialiseTests`, `YulEvalTests`, `ParserTests`. It builds and uses the
  `sol-core` and `yule` executables as test tools.
- `sol-core-contract-test` (`test/ContractMain.hs`) — end-to-end contract execution tests via the
  C++ testrunner, wrapping `run_contests.sh`.

Test structure:
- Each test compiles a `.solc` file through the pipeline
- Some tests expect failure (`runTestExpectingFailure`)
- Standard library tests in `std/`

## C++ Testrunner & Integration Tests

### Architecture

The project includes a C++ testrunner (`test/testrunner/`) that executes compiled EVM bytecode using
the evmone EVM implementation. This enables end-to-end integration testing of the full compilation
pipeline.

**Contest test flow:**
```
.solc → sol-core → .hull → yule → .yul → solc → .hex → testrunner → results
```

### Components

**C++ Components:**
- `test/testrunner/testrunner.cpp` - Main test executor
- `test/testrunner/EVMHost.cpp` - EVM state management
- `test/testrunner/CMakeLists.txt` - Build configuration

**Test Scripts:**
- `contest.sh` - Executes single test case through full pipeline
- `run_contests.sh` - Runs all contest test suites
- Test cases in `test/examples/dispatch/*.json` - JSON test specifications

**Dependencies:**
- `boost` - C++ utilities
- `nlohmann_json` - JSON parsing
- `evmone` - EVM implementation (with dependencies: intx, blst)

### Configuration via Environment Variables

The test scripts support configuration through environment variables, allowing them to work both in
local development and Nix builds:

- `SOLCORE_CMD` - Command to run sol-core (default: `"cabal exec sol-core --"`)
- `YULE_CMD` - Command to run yule (default: `"cabal run yule --"`)
- `testrunner_exe` - Path to testrunner binary (default: `"test/testrunner/testrunner"`)
- `evmone` - Path to evmone library (default: `"~/.local/lib/libevmone.so"`)

### Nix Integration

The project uses Nix flakes for reproducible builds:

**Packages** (`nix build .#<package>`):
- `sol-core` - Main Haskell compiler
- `testrunner` - C++ testrunner binary
- `intx`, `blst`, `evmone` - EVM dependencies (built from source)

**Checks** (`nix flake check`):
- `contests` - Builds testrunner and runs integration test suite

The Nix build system:
1. Fetches dependencies (evmone, intx, blst) from upstream Git repositories
2. Patches evmone to disable Hunter package manager (substitutes with Nix-provided deps)
3. Builds testrunner with `pkgs.boost` and `pkgs.nlohmann_json` from nixpkgs
4. Runs contest tests with environment variables pointing to Nix store paths

**Nix derivation files:**
- `nix/evmone.nix` - Builds evmone EVM implementation
- `nix/intx.nix` - Builds extended precision integer library
- `nix/blst.nix` - Builds BLS signature library

## Documentation

- `doc/architecture.md` - Full description of Solcore's compilation-pipeline architecture (see
  "High-Level Architecture" above).
- `doc/` is an `mdbook` book (`doc/src/SUMMARY.md`); serve locally with `mdbook serve doc` or build
  with `mdbook build doc`, published at https://argotorg.github.io/solcore/.
- `doc/module-system.md` - Import/namespace/module-identity specification.
- `doc/comptime*.md` - Design notes for `comptime` semantics (general, integer, string, assembly).
- `doc/specialise.md` - Specialization design notes.
- `spec/` - Pen-and-paper formalization of the type system (LaTeX, built to PDF via CI).
- `doc/railroad/` - Railroad-diagram BNF grammars for Hull and SAIL.

## Working with This Codebase

### When Adding Features

1. **Frontend changes** (syntax, parsing):
   - Update `src/Solcore/Frontend/Syntax/` for AST
   - Update lexer/parser in `src/Solcore/Frontend/Parser/`
   - Update pretty printer in `src/Solcore/Frontend/Pretty/`

2. **Module system changes**:
   - Update `src/Solcore/Frontend/Module/Loader.hs` and `Identity.hs`
   - Update `doc/module-system.md` to keep the spec in sync
   - Add fixtures under `test/imports/`

3. **Type system changes**:
   - Modify type checking logic in `src/Solcore/Frontend/TypeInference/`
   - Update `TcEnv` if environment changes needed
   - Update unification in `TcUnify.hs` if type structure changes

4. **Desugaring changes**:
   - Add new desugaring pass to `src/Solcore/Desugarer/`
   - Decide: Should it run BEFORE or AFTER type checking?
     - **Early desugaring** (before type checking): Use for syntax simplification, instance
       derivation, or defunctionalization that doesn't need type info
     - **Late desugaring** (after type checking): Use when you need type information to guide the
       transformation
   - Register in the per-module pipeline (`prepareInferenceDeclsForTypeInference`) or the
     post-typecheck pipeline in `SolcorePipeline.hs`, at the appropriate position
   - Order matters: some passes depend on others (e.g., match compiler needs if-desugaring first)

5. **Mast / comptime evaluation changes**:
   - Update `src/Solcore/Backend/Mast.hs` for the IR itself
   - Update `src/Solcore/Backend/Specialise.hs` for how Mast is produced
   - Update `src/Solcore/Backend/MastEval.hs` for partial evaluation/dead-code elimination
   - Update `src/Solcore/Backend/ComptimeCheck.hs` for comptime annotation checking

6. **Hull IR / Yul backend changes**:
   - Update `src/Language/Hull.hs` / `Language/Hull/Types.hs` for the IR
   - Update emission in `src/Solcore/Backend/EmitHull.hs`
   - Update Yul translation in `src/Language/Hull/ToYul/Translate.hs`

7. **Diagnostics changes**:
   - Update `src/Solcore/Diagnostics.hs` for new diagnostic codes/rendering
   - Update the label-inference heuristics in `SolcorePipeline.hs` if a new diagnostic needs a
     source span attached automatically

### Important Design Constraints

- **Pipeline order matters**: Each pass expects certain invariants from previous passes
- **Early vs late desugaring split**:
  - Early desugaring (pre-typecheck) works on untyped, per-module AST (`CompUnit Name`)
  - Late desugaring (post-typecheck) works on the assembled typed AST (`CompUnit Id`)
  - Type checking is the boundary between these two phases
- **Modules are validated/resolved independently, then assembled**: name resolution only sees a
  module's own direct imports; the whole-program view only exists after all modules type-check
- **Contract syntax desugaring happens early**: Field access and dispatch generation run before
  type checking
- **Higher-order elimination happens early**: Defunctionalization occurs before type checking (in
  early desugaring)
- **Whole-program compilation**: Specialization must see all code at once
- **No higher-order functions in Mast**: Defunctionalization eliminates them before type checking
- **Monomorphic Mast**: All type variables must be eliminated by specialization
- **Comptime-only types must not survive to Hull**: `integer` and `string` have no runtime
  representation and must be fully evaluated away by `MastEval` before Hull emission
- **Binary sum encoding**: All sum types encoded as nested `inl/inr` pairs, from Mast through Hull
  to Yul

### Debugging Tips

- Use `ppr` (pretty printer) on AST/Mast/Hull nodes for readable output
- `sol-core` has many `--dump-*` flags (AST, dispatch, desugaring, specialization, Hull) plus
  `--verbose` to print intermediate stages; see `src/Solcore/Pipeline/Options.hs`
- Check intermediate `.hull` files to debug Hull emission
- Check generated `.yul` files to debug Yul translation
- Use `--debug-runtime` with `runsol.sh` for EVM trace visualization
- Type errors come from `TcMonad` - check constraint generation and solving
- If a partial evaluation result looks wrong, check whether it ran out of fuel (`--pe-fuel N`) or
  whether the relevant primitive is handled in `Solcore.Backend.MastEval.evalPrimitive`

### Common Gotchas

- **Name shadowing**: The compiler uses unique IDs (`Id`) not raw names after type checking
- **Type variable scoping**: Be careful with `Forall` quantification and skolemization
- **Specialization dependencies**: Recursive specialization can create new specialization work
- **Sum type tag ordering**: Must match between Mast/Hull emission and Yul translation
- **Memory layout**: Yul translation assumes specific layouts for products and sums
- **Module identity vs. file path**: two different logical module paths that happen to resolve to
  the same physical file are still different modules; don't assume file-path equality implies
  module equality
