#!/usr/bin/env bash

set -euo pipefail

# Check for input file
if [[ $# -lt 1 ]]; then
    echo "Usage: $0 <file.solc> [options]"
    echo "Options:"
    echo "  --calldata <sig> [args...]  Generate calldata using cast calldata"
    echo "  --raw-calldata <hex>         Pass raw calldata directly to hevm"
    echo "  --callvalue <value>         Pass callvalue to hevm (in wei)"
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
base=$(basename "$file" .solc)
core=build/output1.core
hexfile=build/$base.hex
yulfile=build/$base.yul

# Parse arguments
calldata_sig=""
calldata_args=()
raw_calldata=""
callvalue=""

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

# Build and execute hevm command
hevm_cmd="hevm exec --code-file $hexfile"

if [[ -n "$calldata_sig" ]]; then
    if ! calldata=$(cast calldata "$calldata_sig" "${calldata_args[@]}"); then
        echo "Error: Failed to generate calldata"
        exit 1
    fi
    hevm_cmd="$hevm_cmd --calldata $calldata"
elif [[ -n "$raw_calldata" ]]; then
    hevm_cmd="$hevm_cmd --calldata $raw_calldata"
fi

if [[ -n "$callvalue" ]]; then
    hevm_cmd="$hevm_cmd --value $callvalue"
fi

echo "Executing..."
output=$(eval "$hevm_cmd" 2>&1) || true
echo "$output"

# Check if output is a Return value and attempt to decode it
if [[ "$output" =~ ^\"?Return:\ (0x[0-9a-fA-F]+)\"?$ ]]; then
    return_data="${BASH_REMATCH[1]}"
    if [[ ${#return_data} -eq 66 ]]; then  # 0x + 64 hex chars = 32 bytes
        echo "Decoded: $(cast --to-dec "$return_data")"
    fi
fi
