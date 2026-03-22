# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Solcore is an experimental implementation of Solidity's new type system. It's a prototype compiler that produces executable EVM code. The compiler implements a sophisticated type system with parametric polymorphism (generics) and type classes (similar to Haskell), which are compiled down to monomorphic code through specialization. This is later translated into lower level language (basically Yul with sums and products), from which Yul code is generated. So far we rely on external tools to translate Yul code to EVM bytecode.

**Important**: This is a research prototype, not production-ready. It contains bugs and is not optimized for UX.

## Build & Development Commands

### Setup
```bash
# Enter development shell with all dependencies (recommended)
nix develop

# If nix flakes give errors, add to ~/.config/nix/nix.conf:
# experimental-features = nix-command flakes
```

The development shell includes:
- Haskell tools: GHC 9.8, cabal, HLS
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

# Run specific test - the test suite uses tasty, individual tests can be filtered
cabal test --test-options="-p 'pattern'"

# Build C++ testrunner (required for contest integration tests)
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

The compiler is split into **two separate binaries**:

1. **sol-core**: Typechecks, specializes, and lowers to Core IR
2. **yule**: Translates Core IR to Yul

```bash
# Compile .solc source to .core IR
cabal run sol-core -- -f <input.solc>
# Produces: output1.core

# Translate .core to .yul
cabal run yule -- output1.core -o output.yul

# Optional: skip deployment code generation
cabal run yule -- output1.core -o output.yul --nodeploy
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

### Compilation Pipeline Flow

```
Source (.solc) → Parser → AST → Early Desugaring → Type Checker → Late Desugaring → Core IR → Yul
                          ↑                                                            ↑
                                          Frontend (sol-core)                    Backend (yule)
```

The pipeline consists of 13 sequential passes (see `SolcorePipeline.hs:67-168`):

**Phase 1: Parsing & Early Desugaring (Untyped AST)**
1. **Parsing** → Parse source to untyped AST
2. **Name Resolution** → Resolve names and build AST (`CompUnit Name`)
3. **Field Access Desugaring** → Desugar contract field access syntax
4. **Contract Dispatch Generation** → Generate method dispatch code for contracts
5. **SCC Analysis** → Analyze strongly connected components for mutual recursion
6. **Indirect Call Handling** → Defunctionalization (eliminate higher-order functions)
7. **Wildcard Replacement** → Replace pattern wildcards with fresh variables
8. **Function Type Argument Elimination** → Remove function-typed parameters

**Phase 2: Type Checking**
9. **Type Inference** → Constraint-based bidirectional type checking → Typed AST (`CompUnit Id`)

**Phase 3: Late Desugaring & Lowering (Typed AST)**
10. **If/Bool Desugaring** → Lower if-expressions to pattern matching on sum types
11. **Match Compilation** → Compile complex patterns to simple case trees (Augustsson's algorithm)
12. **Specialization** → Monomorphization of polymorphic/overloaded code
13. **Core Emission** → Translate to Core IR (first-order functional IR)

**Phase 4: Yul Translation (Separate Binary)**
14. **Yul Translation** (`yule` binary) → Translate Core IR to Yul code (EVM-oriented assembly)

**Key Insight**: Early desugaring (steps 3-8) simplifies contract-specific syntax and higher-order constructs BEFORE type checking. This allows the type checker to work on a simpler, more uniform AST. Late desugaring (steps 10-11) handles constructs that benefit from type information.

### Key Modules

**Pipeline Orchestration**:
- `src/Solcore/Pipeline/SolcorePipeline.hs` - Main compilation pipeline (orchestrates all 13 passes)

**Phase 1: Parsing & Early Desugaring (Untyped)**:
- `src/Solcore/Frontend/Parser/` - Lexer and parser
- `src/Solcore/Frontend/Syntax/` - AST definitions
- `src/Solcore/Desugarer/FieldAccess.hs` - Field access desugaring
- `src/Solcore/Desugarer/ContractDispatch.hs` - Contract method dispatch generation
- `src/Solcore/Frontend/TypeInference/SccAnalysis.hs` - Dependency analysis
- `src/Solcore/Desugarer/IndirectCall.hs` - Defunctionalization (remove higher-order functions)
- `src/Solcore/Desugarer/ReplaceWildcard.hs` - Wildcard replacement
- `src/Solcore/Desugarer/ReplaceFunTypeArgs.hs` - Function type argument elimination

**Phase 2: Type Checking**:
- `src/Solcore/Frontend/TypeInference/TcContract.hs` - Type checking orchestration
- `src/Solcore/Frontend/TypeInference/TcMonad.hs` - Type checker monad
- `src/Solcore/Frontend/TypeInference/TcEnv.hs` - Type environment
- `src/Solcore/Frontend/TypeInference/TcUnify.hs` - Unification algorithm
- `src/Solcore/Frontend/TypeInference/TcSat.hs` - Type class constraint solving

**Phase 3: Late Desugaring & Lowering (Typed)**:
- `src/Solcore/Desugarer/IfDesugarer.hs` - If-expression desugaring (post-typecheck)
- `src/Solcore/Desugarer/MatchCompiler.hs` - Pattern matching compilation (Augustsson's algorithm)
- `src/Solcore/Desugarer/Specialise.hs` - Monomorphization of polymorphic/overloaded code
- `src/Solcore/Desugarer/EmitCore.hs` - Translation to Core IR
- `src/Language/Core.hs` - Core IR definition

**Phase 4: Yul Backend (Separate Binary)**:
- `yule/Main.hs` - Yule binary entry point
- `yule/Translate.hs` - Core IR to Yul translation
- `yule/TM.hs` - Translation monad
- `yule/Locus.hs` - Location abstraction (stack/memory tracking)

### Type System

The type system implements **HM(X)** with:
- **Parametric polymorphism** (generics with type variables)
- **Type classes** (Haskell-style with multi-parameter type classes)
- **Constraint-based type inference** (bidirectional checking)
- **Instance resolution** with overlapping instance checks

Type checking uses:
- **Inference mode**: Generate constraints from expressions (bottom-up)
- **Checking mode**: Check against expected types (top-down)
- **Constraint solving**: Unification + instance resolution + recursive constraint reduction

### Specialization (Monomorphization)

**Critical phase** that eliminates all polymorphism before code generation:

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

### Data Type Encoding

**Sum types** → Nested binary sums:
- `Either A (Either B C)` becomes `inl a | inr (inl b | inr c)`

**Product types** → Nested pairs:
- `(A, B, C)` becomes `(A, (B, C))`

This uniform encoding allows simple handling at the Core IR level.

### Common Patterns

**Monad Transformers**:
- `TcM = StateT TcEnv (ExceptT String IO)` for type checking
- `SM = StateT SpecState IO` for specialization
- `EM = StateT EcState IO` for Core emission

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

## Test Organization

Tests are organized in `test/examples/`:

- `spec/` - Specification test cases (core language features)
- `cases/` - General test cases
- `dispatch/` - Contract method dispatch tests
- `pragmas/` - Pragma-related tests (bounds checking, coverage, etc.)
- `imports/` - Module import tests

Test framework: **Tasty** with HUnit assertions

Test structure in `test/Main.hs` and `test/Cases.hs`:
- Each test compiles a `.solc` file through the pipeline
- Some tests expect failure (`runTestExpectingFailure`)
- Standard library tests in `std/`

## C++ Testrunner & Integration Tests

### Architecture

The project includes a C++ testrunner (`test/testrunner/`) that executes compiled EVM bytecode using the evmone EVM implementation. This enables end-to-end integration testing of the full compilation pipeline.

**Contest test flow:**
```
.solc → sol-core → .core → yule → .yul → solc → .hex → testrunner → results
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

The test scripts support configuration through environment variables, allowing them to work both in local development and Nix builds:

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

## Working with This Codebase

### When Adding Features

1. **Frontend changes** (syntax, parsing):
   - Update `src/Solcore/Frontend/Syntax/` for AST
   - Update lexer/parser in `src/Solcore/Frontend/Parser/`
   - Update pretty printer in `src/Solcore/Frontend/Pretty/`

2. **Type system changes**:
   - Modify type checking logic in `src/Solcore/Frontend/TypeInference/`
   - Update `TcEnv` if environment changes needed
   - Update unification in `TcUnify.hs` if type structure changes

3. **Desugaring changes**:
   - Add new desugaring pass to `src/Solcore/Desugarer/`
   - Decide: Should it run BEFORE or AFTER type checking?
     - **Early desugaring** (before type checking): Use for syntax simplification that doesn't need type info
     - **Late desugaring** (after type checking): Use when you need type information to guide transformation
   - Register in pipeline in `SolcorePipeline.hs` at the appropriate position
   - Order matters: some passes depend on others (e.g., match compiler needs if-desugaring first)

4. **Core IR changes**:
   - Update `src/Language/Core.hs`
   - Update emission in `src/Solcore/Desugarer/EmitCore.hs`
   - Update Yul translation in `yule/Translate.hs`

### Important Design Constraints

- **Pipeline order matters**: Each pass expects certain invariants from previous passes
- **Early vs late desugaring split**:
  - Early desugaring (pre-typecheck) works on untyped AST (`CompUnit Name`)
  - Late desugaring (post-typecheck) works on typed AST (`CompUnit Id`)
  - Type checking is the boundary between these two phases
- **Contract syntax desugaring happens early**: Field access and dispatch generation run before type checking
- **Higher-order elimination happens early**: Defunctionalization occurs before type checking (in early desugaring)
- **Whole-program compilation**: Specialization must see all code at once
- **No higher-order functions in Core**: Defunctionalization eliminates them before type checking
- **Monomorphic Core**: All type variables must be eliminated by specialization
- **Binary sum encoding**: All sum types encoded as nested `inl/inr` pairs

### Debugging Tips

- Use `ppr` (pretty printer) on AST nodes for readable output
- Check intermediate `.core` files to debug Core emission
- Check generated `.yul` files to debug Yul translation
- Use `--debug-runtime` with `runsol.sh` for EVM trace visualization
- Type errors come from `TcMonad` - check constraint generation and solving

### Common Gotchas

- **Name shadowing**: The compiler uses unique IDs (`Id`) not raw names after type checking
- **Type variable scoping**: Be careful with `Forall` quantification and skolemization
- **Specialization dependencies**: Recursive specialization can create new specialization work
- **Sum type tag ordering**: Must match between Core emission and Yul translation
- **Memory layout**: Yul translation assumes specific layouts for products and sums
