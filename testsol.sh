function esolc() {
    file=$1
    echo $file
    shift
    cabal run sol-core -- -f $file $*
}

function testsol() {
    file=$1
    echo $file
    shift
    cabal run sol-core -- -f $file $* && \
	cabal exec yule -- output.core -w -O > /dev/null && \
    forge script Output.sol | grep RESULT
}

function testspec() {
    file=$1
    echo $file
    shift
    cabal exec sol-core -- -f $file --debug-spec --dump-spec $*
#    cabal run yule -- output.core -O && \
#    forge script Output.sol
}


function testcore() {
    echo $1
    cabal exec yule -- $1 -w -O && forge script Output.sol | grep RESULT
}

function hevmcore() {
     local base=$(basename $1 .core)
     local yulfile=$base.yul
     echo $yulfile
     cabal exec yule -- $1 -o $yulfile
     local hexfile=$base.hex
     solc --strict-assembly --bin --optimize $yulfile | tail -1 > $hexfile
     hevm exec --code $(cat $hexfile) | awk -f parse_hevm_output.awk
}

function hevmsol() {
    file=$1
    echo $file
    local base=$(basename $1 .solc)
    local core=output.core
    local hexfile=$base.hex
    local yulfile=$base.yul
    echo Hex: $hexfile
    shift
    cabal exec sol-core -- -f $file $* && \
	cabal exec yule -- $core -O -o $yulfile && \
        solc --strict-assembly --bin --optimize $yulfile | tail -1 > $hexfile && \
        hevm exec --code $(cat $hexfile) | awk -f parse_hevm_output.awk

}
