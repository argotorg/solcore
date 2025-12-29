# Experimental compiler for the new Solidity language

## Development

```sh
# enter a development shell with all required dependencies
nix develop

# enter a repl
cabal repl

# build the project
cabal build

# run Haskell unit tests
cabal test

# build the C++ testrunner (for integration tests)
cmake -S . -B build
cmake --build build --target testrunner

# run integration tests (requires testrunner built above)
export testrunner_exe=build/test/testrunner/testrunner
bash run_contests.sh

# run integration tests via Nix (builds everything automatically)
nix flake check

# run the CI pipeline locally (builds sol-core)
nix build
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
# Build the testrunner binary
cmake -S . -B build
cmake --build build --target testrunner
# Creates: build/test/testrunner/testrunner
```

### Running Integration Tests

**Option 1: Manual execution (requires testrunner built above)**
```bash
export testrunner_exe=build/test/testrunner/testrunner
bash run_contests.sh
```

**Option 2: Via Nix (recommended - builds everything automatically)**
```bash
nix flake check
```

The Nix approach automatically:
- Builds the testrunner with all dependencies (evmone, intx, blst)
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
