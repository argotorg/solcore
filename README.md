# Experimental Core Solidity Compiler

This is a prototype implementation of Solidity's new type system. It is functional and can produce
executable EVM code. It has not been optimized for UX, and if you're looking for an industrial
compiler with a clean UX and good error messages you will not find it here. The compiler contains
bugs. You should under no circumstances be using this in a production setting.

## Documentation

- A high level overview of the language and it's design can be found in the [Core Solidity Deep
    Dive](https://www.solidity.org/blog/2025/11/14/core-solidity-deep-dive/) post on the Solidity
    blog.
- Work in progress reference documentation is published at https://argotorg.github.io/solcore/.
- A pen and paper formalization of the type system is
    defined in the `./spec` directory, and pdf builds are available as artifacts from the [Spec PDF
    workflow](https://github.com/argotorg/solcore/actions/workflows/spec.yml?query=branch%3Amain).
- The implemented module and namespace behavior is documented in [`doc/module-system.md`](doc/module-system.md).

Bug reports and feedback are very welcome.

## Development

```sh
# enter a development shell with all required dependencies
nix develop

# enter a repl
cabal repl

# build the project
cabal build

# run unit tests only
cabal test sol-core-tests

# run contract integration tests through cabal
# (auto-builds testrunner, auto-detects evmone; fails if deps are missing)
cabal test sol-core-contract-test

# run every test suite
cabal test

# build the C++ testrunner (for integration tests)
cmake -S . -B build
cmake --build build --target testrunner

# run integration tests directly (requires evmone)
bash run_contests.sh

# run integration tests via Nix (builds everything automatically)
nix flake check

# run the CI pipeline locally (builds sol-core)
nix build

# run all checks (including ormolu format check)
nix flake check

# format all Haskell files with ormolu.
ormolu --mode inplace $(find app src yule test -name '*.hs')
```

## Using nix and flakes

This project uses nix flakes. If you haven't already you should add the following to
`~/.config/nix/nix.conf` (user local) or `/etc/nix/nix.conf` (system wide) to disable errors about
experimental-features:

```
experimental-features = nix-command flakes
```

Alternatively, the [determinate nix installer](https://determinate.systems/nix-installer/) installs
nix with flakes enabled automatically.

# Usage

## Compilation

The compiler is currented implemented as two binaries:

1. `sol-core`: typechecks, specializes, and lowers to the `core` IR
2. `yule`: lowers `core` files to `yul`

```
# produces `output1.core`
$ cabal run -- sol-core -f <input>

# produces an output.yul
$ cabal run -- yule output1.core -o output.yul
```

## Running Code

The `runsol.sh` script implements a small pipeline that executes a core solidity contract by
compiling via `sol-core` -> `yule` -> `solc`, and then using `geth` to execute the resulting EVM
code.

It takes the following arguments:

```
> ./runsol.sh
Options:
  --runtime-calldata sig [args...]  Generate calldata using cast calldata
  --runtime-raw-calldata hex        Pass raw calldata directly to geth
  --runtime-callvalue value         Pass callvalue to geth (in wei)
  --debug-runtime                   Explore the evm execution in the interactive debugger
  --create true|false               Run the initcode to deploy the contract (default: true)
  --create-arguments sig [args...]  Generate calldata using cast calldata
  --create-raw-arguments hex        Pass raw calldata directly to geth
  --create-callvalue value          Pass callvalue to geth (in wei)
  --debug-create                    Explore the evm execution in the interactive debugger
```

## Integration Tests

The project includes a C++ testrunner that executes end-to-end integration tests by running compiled bytecode on the evmone EVM implementation. These tests verify the full compilation pipeline:

```
.solc → sol-core → .core → yule → .yul → solc → .hex → testrunner → results
```

### Building the Testrunner

The testrunner requires cmake and boost, which are available in the `nix develop` shell:

```bash
# If the repo was cloned without submodules, initialize JSON dependency:
git submodule update --init deps/nlohmann_json

# Build the testrunner binary
cmake -S . -B build
cmake --build build --target testrunner
# Creates: build/test/testrunner/testrunner
```

### Running Integration Tests

**Option 1: Via cabal (recommended for local development)**
```bash
cabal test sol-core-contract-test
```

The cabal suite:
- Reuses `run_contests.sh`
- Builds `testrunner` automatically if missing
- Auto-detects `libevmone` from common locations
- Fails by default if dependencies are unavailable

Set `SOLCORE_CONTRACT_TESTS_ALLOW_SKIP=1` only if you explicitly want dependency-related skips.

**Option 2: Manual execution**
```bash
bash run_contests.sh
```

Manual mode is strict: if `testrunner` or `evmone` is missing, the script fails.

**Option 3: Via Nix (builds everything automatically)**
```bash
nix flake check
```

The Nix approach automatically:
- Builds the testrunner with all dependencies (including evmone with pinned intx/blst)
- Compiles test contracts through the full pipeline
- Executes tests and verifies results

### Test Cases

Integration test cases are located in `test/examples/dispatch/` as JSON files that specify:
- Input contract (`.solc` file)
- Test scenarios with input/output expectations
- Expected EVM execution results

The `contest.sh` script can also be used to run individual test cases:
```bash
bash contest.sh test/examples/dispatch/basic.json
```

### Documentation

To work on the language reference, enter the nix development shell and run:

```sh
# serve the docs on localhost (rebuilds when files change)
mdbook serve doc

# build a static copy of the docs
mdbook build doc
```
