# Experimental compiler for the new Solidity language

# Usage
## Compiling

    $ cabal exec -- sol-core -f test/examples/spec/022add.solc

(writes to output.core)

## Running

### Using HEVM

    $ cabal run yule -- output.core -o Input.yul
    $ solc --strict-assembly --bin --optimize Input.yul | tail -1 > Output.hex
    $ hevm exec --code $(cat Output.hex) | awk -f parse_hevm_output.awk
    Return: 42
    hex: 0x000000000000000000000000000000000000000000000000000000000000002a

The file `testsol.sh` defines bash functions `hevmcore` and `hevmsol` that perform these steps from core and solc source respectively.

### Using Foundry

    $ cabal run yule -- output.core -w -o Output.sol
      found main
      writing output to Output.sol
    $ forge script Output.sol
    Script ran successfully.
    Gas used: 24910

    == Logs ==
    RESULT -->  42


The file `testsol.sh` defines bash functions `testcore` and `testsol` that perform these steps from core and solc source respectively.
