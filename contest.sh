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

# Allow overriding yule command (useful for Nix builds)
: ${YULE_CMD:="cabal run exe:yule --"}

# Compile one .solc source into its runtime hex (stdout = hex only; the
# sol-core/yule chatter goes to stderr so it isn't captured).
build_hex() {
    local s="$1" sub="$2"
    mkdir -p "$sub"
    if ! $SOLCORE_CMD -f "$s" -o "$sub" >&2; then echo "Error: sol-core failed for $s" >&2; return 1; fi
    if [[ ! -f "$sub/output1.hull" ]]; then echo "Error: no output1.hull for $s" >&2; return 1; fi
    local y="$sub/output.yul"
    local yargs=("$sub/output1.hull" -o "$y")
    if [[ "$create" == "false" ]]; then yargs+=(--nodeploy); fi
    if ! $YULE_CMD "${yargs[@]}" >&2; then echo "Error: yule failed for $s" >&2; return 1; fi
    solc --strict-assembly --bin --optimize "$y" 2>/dev/null | tail -1 | tr -d '\n'
}

if [[ "$(jq -r ".$suite | has(\"artifactSources\")" "$file")" == "true" ]]; then
    # Multi-contract suite: compile each named artifact and inject its hex into
    # the suite's "artifacts" map, so the runner can deploy them and wire them
    # together by address (e.g. a diamond and its facets).
    echo "Compiling artifacts..."
    cp "$file" "$runner_input"
    for name in $(jq -r ".$suite.artifactSources | keys[]" "$file"); do
        srcfile=$(jq -r ".$suite.artifactSources[\"$name\"]" "$file")
        echo "  artifact $name <- $srcfile"
        hex=$(build_hex "$test_dir/$srcfile" "$work_dir/art_$name") || exit 1
        tmp="$work_dir/ri_$name.json"
        jq --arg n "$name" --arg h "$hex" ".${suite}.artifacts[\$n] = \$h" "$runner_input" > "$tmp" && mv "$tmp" "$runner_input"
    done
else
    # Single-contract suite (legacy): compile $src into the "bytecode" field.
    # Keep the exact `jq <filter> <file>` positional form the concurrency test's
    # fake jq relies on (test_contest_concurrency.sh).
    echo "Compiling to Hull..."
    hex=$(build_hex "$src" "$work_dir") || exit 1
    printf '%s' "$hex" > "$hexfile"
    echo "Hex output: $hexfile"
    jq ".$suite.bytecode |= \"$hex\" " "$file" > "$runner_input"
fi

"$testrunner_exe" "$evmone" "$runner_input" "$runner_output"
