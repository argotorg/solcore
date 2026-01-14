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
    rm -f -v output1.hull Output.sol
    /usr/bin/time -f "Compilation time: %E" cabal run sol-core -- -f $file $* && \
	cabal exec yule -- output1.hull -w -O --nodeploy > /dev/null && \
        forge script --via-ir Output.sol | egrep '(Gas|RESULT)'
}


function runhull() {
    echo $1
    rm -f -v Output.sol
    cabal exec yule -- $1 -w --nodeploy -O && forge script --via-ir Output.sol | egrep '(Gas|RESULT)'
}

function hevmhull() {
     local base=$(basename $1 .hull)
     local yulfile=$base.yul
     echo $yulfile
     local hexfile=$base.hex
     rm -f -v $yulfile $hexfile
     cabal exec yule -- $1 --nodeploy -o $yulfile
     hex=$(solc --strict-assembly --bin --optimize --optimize-yul $yulfile | tail -1)
     hevm exec --code $hex | awk -f parse_hevm_output.awk
}

function hevmsol() {

    echo $*
    file=$1
    echo $file
    local base=$(basename $1 .solc)
    local hull=output1.hull
    local hexfile=$base.hex
    local yulfile=$base.yul
    echo Hex: $hexfile
    shift
    cabal exec sol-core -- -f $file $* && \
	cabal exec yule -- $hull --nodeploy -O -o $yulfile && \
        solc --strict-assembly --bin --optimize $yulfile | tail -1 > $hexfile && \
        hevm exec --code $(cat $hexfile) | awk -f parse_hevm_output.awk

}

function hevmsol() {
    echo $*
    file=$1
    echo $file
    local base=$(basename $1 .solc)
    local hull=output1.hull
    local hexfile=$base.hex
    local yulfile=$base.yul
    echo Hex: $hexfile
    shift
    cabal exec sol-core -- -f $file $* && \
	cabal exec yule -- $hull --nodeploy -O -o $yulfile && \
        solc --strict-assembly --bin --optimize $yulfile | tail -1 > $hexfile && \
        hevm exec --code $(cat $hexfile) | awk -f parse_hevm_output.awk

}

function solchex() {
    local file=$1
    shift
    echo "Solc: $file"
    local base=$(basename $file .solc)
    local hull=output1.hull
    echo "Hull: $hull"
    local yulfile=$base.yul
    echo "Yul:  $yulfile"
    rm -f -v $yulfile
    cabal exec sol-core -- -f $file $* && \
    cabal exec yule -- $hull -o $yulfile
    hex=$(solc --strict-assembly --bin --optimize --optimize-yul $yulfile | tail -1)
    local hexfile=$base.hex
    echo "Writing hex:  $hexfile"
    echo $hex > $hexfile
}

function deployhull() {
    local base=$(basename $1 .hull)
    local yulfile=$base.yul
    echo $yulfile
    local hexfile=$base.hex
    rm -f -v $yulfile $hexfile
    cabal exec yule -- $1 -o $yulfile
    hex=$(solc --strict-assembly --bin --optimize --optimize-yul $yulfile | tail -1)
    rawtx=$(cast mktx --private-key=$DEPLOYER_KEY --create $hex)
    addr=$(cast publish $rawtx | jq .contractAddress | tr -d '"')
    echo $addr
}

function deployyul() {
    local yulfile=$1
    shift
    local data=$(cast ae $* | cut -c 3-)
    local base=$(basename $yulfile .yul)
    echo "Args: $*"
    echo "ABI-enc: $data"
    prog=$(solc --strict-assembly --bin --optimize --optimize-yul $yulfile | tail -1)
    hex="$prog$data"
    echo Hex: $hex
    rawtx=$(cast mktx --private-key=$DEPLOYER_KEY --create $hex $*)
    txoutput=$(cast publish $rawtx)
    echo $txoutput | jq .
    export contractAddress=$(echo $txoutput | jq .contractAddress | tr -d '"')
    echo $contractAddress
}

function deployhex() {
    local hexfile=$1
    shift
    local data=$(cast ae $* | cut -c 3-)
    echo "Args: $*"
    echo "ABI-enc: $data"
    prog=$(cat $hexfile)
    hex="$prog$data"
    echo Hex: $hex
    rawtx=$(cast mktx --private-key=$DEPLOYER_KEY --create $hex $*)
    txoutput=$(cast publish $rawtx)
    echo $txoutput | jq .
    export contractAddress=$(echo $txoutput | jq .contractAddress | tr -d '"')
    echo $contractAddress
}

# deploy contract with 1 uint arg
function deployyul1() {
    local yulfile=$1
    local data=$(cast ae "constructor(uint256)" $2 | cut -c 3-)
    local base=$(basename $1 .yul)
    prog=$(solc --strict-assembly --bin --optimize --optimize-yul $yulfile | tail -1)
    hex="$prog$data"
    echo Hex: $hex
    rawtx=$(cast mktx --private-key=$DEPLOYER_KEY --create $hex)
    txoutput=$(cast publish $rawtx)
    echo $txoutput | jq .
    export contractAddress=$(echo $txoutput | jq .contractAddress | tr -d '"')
    echo $contractAddress
}

function hull() {
     local base=$(basename $1 .hull)
     local yulfile=$base.yul
     rm -f -v $yulfile
     cabal exec yule -- $1 -o $yulfile
}
