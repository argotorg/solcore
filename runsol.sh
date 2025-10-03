#!/usr/bin/env bash

set -euo pipefail

# Check for input file
if [[ $# -lt 1 ]]; then
    echo "Usage: $0 file.solc [options]"
    echo "Options:"
    echo "  --calldata sig [args...]  Generate calldata using cast calldata"
    echo "  --raw-calldata hex        Pass raw calldata directly to geth"
    echo "  --callvalue value         Pass callvalue to geth (in wei)"
    echo "  --debug                   Explore the evm execution in the interactive debugger"
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
build_dir="$root_dir/build"
base=$(basename "$file" .solc)
core="$build_dir/output1.core"
hexfile="$build_dir/$base.hex"
yulfile="$build_dir/$base.yul"
tracefile="$build_dir/trace.jsonl"

# Parse arguments
calldata_sig=""
calldata_args=()
raw_calldata=""
callvalue=""
debug=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --calldata)
            shift
            if [[ $# -eq 0 ]]; then
                echo "Error: --calldata requires a signature"
                exit 1
            fi
            calldata_sig=$1
            shift
            # Collect arguments until next flag or end
            while [[ $# -gt 0 ]] && [[ ! "$1" =~ ^-- ]]; do
                calldata_args+=("$1")
                shift
            done
            ;;
        --raw-calldata)
            shift
            if [[ $# -eq 0 ]]; then
                echo "Error: --raw-calldata requires a hex value"
                exit 1
            fi
            raw_calldata=$1
            shift
            ;;
        --callvalue)
            shift
            if [[ $# -eq 0 ]]; then
                echo "Error: --callvalue requires a value"
                exit 1
            fi
            callvalue=$1
            shift
            ;;
        --debug)
            shift
            debug=true
            ;;
        *)
            echo "Error: Unknown option: $1"
            exit 1
            ;;
    esac
done

# Check for conflicting flags
if [[ -n "$calldata_sig" ]] && [[ -n "$raw_calldata" ]]; then
    echo "Error: Cannot use both --calldata and --raw-calldata"
    exit 1
fi

# Execute compilation pipeline
echo "Compiling to core..."
if ! cabal run sol-core -- -f "$file"; then
    echo "Error: sol-core compilation failed"
    exit 1
fi

mkdir -p build
if ls ./output*.core 1> /dev/null 2>&1; then
    mv ./output*.core build/
fi

echo "Generating Yul..."
if ! cabal run yule -- "$core" -o "$yulfile"; then
    echo "Error: yule generation failed"
    exit 1
fi

echo "Compiling to bytecode..."
if ! solc --strict-assembly --bin --optimize "$yulfile" | tail -1 > "$hexfile"; then
    echo "Error: solc compilation failed"
    exit 1
fi

echo "Hex output: $hexfile"

# Build and execute evm command
evm_cmd="evm run --trace --trace.nomemory=false --trace.noreturndata=false --codefile $hexfile"

if [[ -n "$calldata_sig" ]]; then
    if ! calldata=$(cast calldata "$calldata_sig" "${calldata_args[@]}"); then
        echo "Error: Failed to generate calldata"
        exit 1
    fi
    evm_cmd="$evm_cmd --input $calldata"
elif [[ -n "$raw_calldata" ]]; then
    evm_cmd="$evm_cmd --input $raw_calldata"
fi

if [[ -n "$callvalue" ]]; then
    evm_cmd="$evm_cmd --value $callvalue"
fi

echo "Executing..."
output=$(eval "$evm_cmd" 2>&1) || true
echo "$output" > "$tracefile"

result=$(jq -sr 'last | .output' "$tracefile")
error=$(jq -sr 'last | .error' "$tracefile")

if [[ "$debug" == "true" ]]; then
    traceview "$tracefile"
fi

if [[ "$error" == "null" ]]; then
    if [[ -n "$calldata_sig" ]]; then
        echo "Execution successful"
        # Remove quotes from result and decode
        if [[ -n "$result" ]]; then
            decoded=$(cast abi-decode "$calldata_sig" "0x$result")
            echo "Decoded output: $decoded"
        else
            echo "No return data to decode"
        fi
    else
      echo "Execution successful"
      echo "returndata: 0x${result}"
    fi
else
    echo "Execution failed: $error"
    echo "returndata: 0x$result"
fi
