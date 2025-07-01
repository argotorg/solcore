Compiles core language to Yul wrapped in a Solidity contract,
ready to be run with forge

## Example

```
$ cabal run -- yule
Missing: FILE

Usage: yule FILE [-c|--contract NAME] [-o|--output FILE] [-v|--verbose]

  Compile a Core program to Yul

$ cabal run -- yule examples/core/02sum.core --contract Sum -o Sum.sol
writing output to Sum.sol

$ forge script Sum.sol
[.] Compiling...
[.] Compiling 1 files with 0.8.23
[.] Solc 0.8.23 finished in 284.26ms
Compiler run successful!
Script ran successfully.
Gas used: 24357

== Logs ==
  RESULT -->  42
```