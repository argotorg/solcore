function esolc() {
    file=$1
    echo $file
    shift
    cabal exec sol-core -- -f $file $*
}

function testsol() {
    file=$1
    echo $file
    shift
    rm -f -v output1.core Output.sol
    /usr/bin/time -f "Compilation time: %E" cabal run sol-core -- -f $file $* && \
	cabal exec yule -- output1.core -w -O > /dev/null && \
        forge script --via-ir Output.sol | egrep '(Gas|RESULT)'
}

function testspec() {
    file=$1
    echo $file
    shift
    rm -f -v output1.core
    cabal exec sol-core -- -f $file --debug-spec --dump-spec $*
#    cabal run yule -- output1.core -O && \
#    forge script Output.sol
}


function testcore() {
    echo $1
    rm -f -v Output.sol
    cabal exec yule -- $1 -w -O && forge script --via-ir Output.sol | egrep '(Gas|RESULT)'
}

function hevmcore() {
     local base=$(basename $1 .core)
     local yulfile=$base.yul
     echo $yulfile
     local hexfile=$base.hex
     rm -f -v $yulfile $hexfile
     cabal exec yule -- $1 -o $yulfile
     solc --strict-assembly --bin --optimize $yulfile | tail -1 > $hexfile
     hevm exec --code $(cat $hexfile) | awk -f parse_hevm_output.awk
}

function hevmsol() {
    file=$1
    echo $file
    local base=$(basename $1 .solc)
    local core=output1.core
    local hexfile=$base.hex
    local yulfile=$base.yul
    echo Hex: $hexfile
    shift
    cabal exec sol-core -- -f $file $* && \
	cabal exec yule -- $core -O -o $yulfile && \
        solc --strict-assembly --bin --optimize $yulfile | tail -1 > $hexfile && \
        hevm exec --code $(cat $hexfile) | awk -f parse_hevm_output.awk

}
