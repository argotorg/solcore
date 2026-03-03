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
root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
test_dir=$(dirname $file)
build_dir="$root_dir/build"
base=$(basename "$file" .json)
src="$test_dir/$base.solc"
hull="$build_dir/output1.hull"
hexfile="$build_dir/$base.hex"
yulfile="$build_dir/$base.yul"

create=true
# Allow overriding testrunner location (useful for Nix builds)
if [[ -z "${testrunner_exe:-}" ]]; then
    if [[ -x "$root_dir/build/test/testrunner/testrunner" ]]; then
        testrunner_exe="$root_dir/build/test/testrunner/testrunner"
    else
        testrunner_exe="$root_dir/test/testrunner/testrunner"
    fi
fi

# Allow overriding evmone location (useful for Nix builds)
if [[ -z "${evmone:-}" ]]; then
    evmone_candidates=(
        "$root_dir/result/lib/libevmone.so"
        "$HOME/.local/lib/libevmone.so"
        "$HOME/.local/lib/libevmone.dylib"
        "/opt/homebrew/lib/libevmone.dylib"
        "/usr/local/lib/libevmone.dylib"
        "/usr/local/lib/libevmone.so"
        "/lib/x86_64-linux-gnu/libevmone.so"
        "/usr/lib/x86_64-linux-gnu/libevmone.so"
        "/usr/lib/libevmone.so"
    )
    for candidate in "${evmone_candidates[@]}"; do
        if [[ -f "$candidate" ]]; then
            evmone="$candidate"
            break
        fi
    done
fi

if [[ -z "${evmone:-}" ]]; then
    echo "Error: libevmone not found. Set evmone=/path/to/libevmone.{so,dylib}"
    exit 1
fi

if [[ ! -f "$evmone" ]]; then
    echo "Error: evmone library not found at '$evmone'"
    exit 1
fi

if [[ ! -x "$testrunner_exe" ]]; then
    echo "Error: testrunner not found or not executable at '$testrunner_exe'"
    exit 1
fi

presuite=$(jq keys[0] $file)
suite=$(echo $presuite | tr -d '"')

#echo json: $file
#echo src: $src
#echo hex: $hexfile
#echo suite: $suite

# Execute compilation pipeline
echo "Compiling to Hull..."
# Allow overriding sol-core command (useful for Nix builds)
: ${SOLCORE_CMD:="cabal run exe:sol-core --"}
if ! $SOLCORE_CMD -f "$src"; then
    echo "Error: sol-core compilation failed"
    exit 1
fi

mkdir -p "$build_dir"
if ls ./output*.hull 1> /dev/null 2>&1; then
    mv ./output*.hull "$build_dir"/
fi

if [[ ! -f "$hull" ]]; then
    echo "Error: sol-core did not produce output1.hull"
    exit 1
fi

echo "Generating Yul..."
# Allow overriding yule command (useful for Nix builds)
: ${YULE_CMD:="cabal run exe:yule --"}
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

jq ".$suite.bytecode |= \"$(cat $hexfile)\" " $file > $build_dir/$suite.json

"$testrunner_exe" "$evmone" "$build_dir/$suite.json" "$build_dir/$suite-output.json"
