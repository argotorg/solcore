#!/usr/bin/env bash

set -euo pipefail

# Run an integration test described in a JSON file

# Check for input file
if [[ $# -lt 1 ]]; then
    echo "Usage: $0 file.json [options]"
    exit 1
fi

# Setup file paths
file=$1
shift

if [[ ! -f "$file" ]]; then
    echo "Error: File '$file' not found"
    exit 1
fi

echo "Processing: $file"
root_dir="$(cd "$(dirname "$(readlink --canonicalize "${BASH_SOURCE[0]}")")" && pwd)"
test_dir=$(dirname $file)
build_dir="$root_dir/build"
base=$(basename "$file" .json)
src="$test_dir/$base.solc"
hull="$build_dir/output1.core"
hexfile="$build_dir/$base.hex"
yulfile="$build_dir/$base.yul"

create=true
# Allow overriding evmone location (useful for Nix builds)
: ${evmone:=~/.local/lib/libevmone.so}
# Allow overriding testrunner location (useful for Nix builds)
: ${testrunner_exe:="test/testrunner/testrunner"}

presuite=$(jq keys[0] $file)
suite=$(echo $presuite | tr -d '"')

#echo json: $file
#echo src: $src
#echo hex: $hexfile
#echo suite: $suite

# Execute compilation pipeline
echo "Compiling to Hull..."
# Allow overriding sol-core command (useful for Nix builds)
: ${SOLCORE_CMD:="cabal exec sol-core --"}
if ! $SOLCORE_CMD -f "$src"; then
    echo "Error: sol-core compilation failed"
    exit 1
fi

mkdir -p build
if ls ./output*.core 1> /dev/null 2>&1; then
    mv ./output*.core build/
fi

echo "Generating Yul..."
# Allow overriding yule command (useful for Nix builds)
: ${YULE_CMD:="cabal run yule --"}
yule_args=("$hull" -o "$yulfile")
if [[ "$create" == "false" ]]; then
    yule_args+=(--nodeploy)
fi
if ! $YULE_CMD "${yule_args[@]}"; then
    echo "Error: yule generation failed"
    exit 1
fi

echo "Compiling to bytecode..."
if ! solc --strict-assembly --bin --optimize "$yulfile" | tail -1 | tr -d '\n' > "$hexfile"; then
    echo "Error: solc compilation failed"
    exit 1
fi

echo "Hex output: $hexfile"

jq ".$suite.bytecode |= \"$(cat $hexfile)\" " $file  > $build_dir/$suite.json

$testrunner_exe $evmone $build_dir/$suite.json $build_dir/$suite-output.json
