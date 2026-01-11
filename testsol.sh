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


function hull2yul() {
     local base=$(basename $1 .hull)
     local yulfile=$base.yul
     rm -f -v $yulfile
     cabal exec yule -- $1 -o $yulfile
}

function deployhex() {
    # Use cast send instead of mktx + publish
    txoutput=$(cast send --private-key=$DEPLOYER_KEY --create $hex --json 2>&1)

    if echo "$txoutput" | grep -qi "error"; then
        echo "Error deploying contract"
        echo "$txoutput"
        return 1
    fi
    echo "Contract deployed"
    # Display transaction hash
    local TX_HASH=$(echo "$txoutput" | jq -r '.transactionHash')
    echo "Transaction Hash: $TX_HASH"

    # Display logs in "topics | value" format, without leading zeroes and quotes
    echo "Logs:"
    echo "$txoutput" | jq -r '.logs[] | "topics: \(.topics | map(gsub("0x0+"; "0x") | gsub("^0x$"; "0x0")) | join(",")) | data: \(.data | gsub("0x0+"; "0x") | gsub("^0x$"; "0x0"))"'

    export contractAddress=$(echo "$txoutput" | jq -r '.contractAddress')
    echo "Contract Address: $contractAddress"
}

function deploysolc() {
    local file=$1
    shift
    local data=$(cast ae $* | cut -c 3-)
    echo "Args: $*"
    #echo "ABI-enc: $data"
    echo "Solc: $file"
    local base=$(basename $file .solc)
    local hull=output1.hull
    echo "Hull: $hull"
    local yulfile=$base.yul
    echo "Yul:  $yulfile"
    rm -f $yulfile
    cabal exec sol-core -- -f $file && \
    cabal exec yule -- $hull -o $yulfile
    prog=$(solc --strict-assembly --bin --optimize --optimize-yul $yulfile | tail -1)
    hex="$prog$data"

    deployhex $hex
}


function deployyul() {
    local yulfile=$1
    shift
    local data=$(cast ae $* | cut -c 3-)
    echo "Args: $*"
    #echo "ABI-enc: $data"
    local base=$(basename $file .yul)
    echo "Yul:  $yulfile"

    prog=$(solc --strict-assembly --bin --optimize --optimize-yul $yulfile | tail -1)
    hex="$prog$data"
    deployhex hex
}

callContract() {
    local signature="$1"
    shift
    local args="$@"

    # First, simulate the call to get the return value
    echo "Function result:"
    local result=$(cast call $contractAddress "$signature" $args 2>&1)

    if echo "$result" | grep -qi "error"; then
	echo "Error calling function: $signature"
	echo "$result"
	return 1
    fi

    echo "$result"

    # Now execute the actual transaction to get logs
    echo -e "\nExecuting transaction..."
    local output=$(cast send $contractAddress "$signature" $args --private-key $DEPLOYER_KEY 2>&1)
    if echo "$output" | grep -qi "error"; then
	echo "Error executing function: $signature"
	echo "$output"
	return 1
     fi

    #local logs=$(echo "$output" | grep -w logs | awk '{print $2}')
    local TX_HASH=$(echo "$output" | grep transactionHash | grep -v logs | awk '{print $2}')

    if [ -z "$TX_HASH" ]; then
	echo "Failed to get transaction hash"
	return 1
    fi
    echo "Contract $contractAddress"
    echo -e "\nLogs:"
    cast receipt $TX_HASH --json | jq '.logs[] | "\(.address) | topics: \(.topics | map(gsub("0x0+"; "0x") | gsub("^0x$"; "0x0")) | join(",")) | data: \(.data | gsub("0x0+"; "0x") | gsub("^0x$"; "0x0"))"' -r
}
