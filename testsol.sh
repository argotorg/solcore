function esolc() {
    file=$1
    echo $file
    shift
    cabal exec sol-core -- -f $file $*
}


function runsol() {
    file=$1
    echo $file
    shift
    rm -f -v output1.core Output.sol
    /usr/bin/time -f "Compilation time: %E" cabal run sol-core -- -f $file $* && \
	cabal exec yule -- output1.core -w -O --nodeploy > /dev/null && \
        forge script --via-ir Output.sol | egrep '(Gas|RESULT)'
}


function runcore() {
    echo $1
    rm -f -v Output.sol
    cabal exec yule -- $1 -w --nodeploy -O && forge script --via-ir Output.sol | egrep '(Gas|RESULT)'
}

function hevmcore() {
     local base=$(basename $1 .core)
     local yulfile=$base.yul
     echo $yulfile
     local hexfile=$base.hex
     rm -f -v $yulfile $hexfile
     cabal exec yule -- $1 --nodeploy -o $yulfile
     solc --strict-assembly --bin --optimize --optimize-yul $yulfile | tail -1 > $hexfile
     hevm exec --code-file $hexfile | awk -f parse_hevm_output.awk
}

function hevmsol() {

    echo $*
    file=$1
    echo $file
    local base=$(basename $1 .solc)
    local core=output1.core
    local hexfile=$base.hex
    local yulfile=$base.yul
    echo Hex: $hexfile
    shift
    cabal exec sol-core -- -f $file $* && \
	cabal exec yule -- $core --nodeploy -O -o $yulfile && \
        solc --strict-assembly --bin --optimize $yulfile | tail -1 > $hexfile && \
        hevm exec --code-file $hexfile | awk -f parse_hevm_output.awk

}

function deploysol() {
    local file=$1
    shift
    echo "Solc: $file"
    local base=$(basename $file .solc)
    local core=output1.core
    echo "Sail: $core"
    local yulfile=$base.yul
    echo "Yul:  $yulfile"
    rm -f -v $yulfile
    cabal exec sol-core -- -f $file $* && \
    cabal exec yule -- $core -o $yulfile
    hex=$(solc --strict-assembly --bin --optimize --optimize-yul $yulfile | tail -1)
    rawtx=$(cast mktx --private-key=$PRIVATE_KEY --create $hex)
    addr=$(cast publish $rawtx | jq .contractAddress)
    echo $addr
}

function deploycore() {
     local base=$(basename $1 .core)
     local yulfile=$base.yul
     echo $yulfile
     local hexfile=$base.hex
     rm -f -v $yulfile $hexfile
     cabal exec yule -- $1 -o $yulfile
     hex=$(solc --strict-assembly --bin --optimize --optimize-yul $yulfile | tail -1)
     rawtx=$(cast mktx --private-key=$PRIVATE_KEY --create $hex)
     addr=$(cast publish $rawtx | jq .contractAddress)
     echo $addr
}

function sail() {
     local base=$(basename $1 .core)
     local yulfile=$base.yul
     rm -f -v $yulfile
     cabal exec yule -- $1 -o $yulfile
}
