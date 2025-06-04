# Experimental compiler for the new Solidity language

## Development

```sh
# enter a development shell with all required dependencies
nix develop

# enter a repl
cabal repl

# build the project
cabal build

# run the tests
cabal test

# run the CI pipeline locally
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