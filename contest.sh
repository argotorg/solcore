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
mkdir -p "$build_dir"
work_root="$build_dir/.contest-work"
mkdir -p "$work_root"
work_dir="$(mktemp -d "$work_root/run.XXXXXX")"
work_marker="$work_dir/.owned-by-contest"
touch "$work_marker"
hull="$work_dir/output1.hull"
yulfile="$work_dir/output.yul"
hexfile="$work_dir/output.hex"
runner_input="$work_dir/runner-input.json"
runner_output="$work_dir/runner-output.json"

cleanup_work_dir() {
    if [[ -z "${work_dir:-}" ]]; then
        return
    fi

    if [[ "$work_dir" != "$work_root"/run.* || ! -f "$work_marker" ]]; then
        echo "Error: refusing to clean unverified contest work directory '$work_dir'" >&2
        return 1
    fi

    rm -rf -- "$work_dir"
}

trap cleanup_work_dir EXIT
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

if ! $SOLCORE_CMD -f "$src" -o "$work_dir"; then
    echo "Error: sol-core compilation failed"
    exit 1
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

jq ".$suite.bytecode |= \"$(cat $hexfile)\" " $file > "$runner_input"

"$testrunner_exe" "$evmone" "$runner_input" "$runner_output"
