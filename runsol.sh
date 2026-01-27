#!/usr/bin/env bash

set -euo pipefail

# Check for input file
if [[ $# -lt 1 ]]; then
    echo "Usage: $0 file.solc [options]"
    echo "Options:"
    echo "  --runtime-calldata sig [args...]  Generate calldata using cast calldata"
    echo "  --runtime-raw-calldata hex        Pass raw calldata directly to geth"
    echo "  --runtime-callvalue value         Pass callvalue to geth (in wei)"
    echo "  --debug-runtime                   Explore the evm execution in the interactive debugger"
    echo "  --create true|false               Run the initcode to deploy the contract (default: true)"
    echo "  --create-arguments sig [args...]  Generate calldata using cast calldata"
    echo "  --create-raw-arguments hex        Pass raw calldata directly to geth"
    echo "  --create-callvalue value          Pass callvalue to geth (in wei)"
    echo "  --debug-create                    Explore the evm execution in the interactive debugger"
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
hull="$build_dir/output1.hull"
hexfile="$build_dir/$base.hex"
yulfile="$build_dir/$base.yul"
runtime_tracefile="$build_dir/trace.runtime.jsonl"
create_tracefile="$build_dir/trace.create.jsonl"
create_poststate="$build_dir/create.poststate.json"

# Parse arguments
runtime_calldata_sig=""
runtime_calldata_args=()
runtime_raw_calldata=""
runtime_callvalue=""
runtime_debug=false

create=true

create_arguments_sig=""
create_args=()
create_raw_args=""
create_callvalue=""
create_debug=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --runtime-calldata)
            shift
            if [[ $# -eq 0 ]]; then
                echo "Error: --runtime-calldata requires a signature"
                exit 1
            fi
            runtime_calldata_sig=$1
            shift
            # Collect arguments until next flag or end
            while [[ $# -gt 0 ]] && [[ ! "$1" =~ ^-- ]]; do
                runtime_calldata_args+=("$1")
                shift
            done
            ;;
        --runtime-raw-calldata)
            shift
            if [[ $# -eq 0 ]]; then
                echo "Error: --runtime-raw-calldata requires a hex value"
                exit 1
            fi
            runtime_raw_calldata=$1
            shift
            ;;
         --runtime-callvalue)
             shift
             if [[ $# -eq 0 ]]; then
                 echo "Error: --runtime-callvalue requires a value"
                 exit 1
             fi
             runtime_callvalue=$1
             shift
             ;;
          --debug-runtime)
              shift
              runtime_debug=true
              ;;
          --create)
              shift
              if [[ $# -eq 0 ]]; then
                  echo "Error: --create requires true or false"
                  exit 1
              fi
              if [[ "$1" == "true" ]]; then
                  create=true
              elif [[ "$1" == "false" ]]; then
                  create=false
              else
                  echo "Error: --create requires true or false, got '$1'"
                  exit 1
              fi
              shift
              ;;
          --create-arguments)
              shift
              if [[ $# -eq 0 ]]; then
                  echo "Error: --create-arguments requires a signature"
                  exit 1
              fi
              create_arguments_sig=$1
              shift
              # Collect arguments until next flag or end
              while [[ $# -gt 0 ]] && [[ ! "$1" =~ ^-- ]]; do
                  create_args+=("$1")
                  shift
              done
              ;;
          --create-raw-arguments)
              shift
              if [[ $# -eq 0 ]]; then
                  echo "Error: --create-raw-arguments requires a hex value"
                  exit 1
              fi
              create_raw_args=$1
              shift
              ;;
          --create-callvalue)
              shift
              if [[ $# -eq 0 ]]; then
                  echo "Error: --create-callvalue requires a value"
                  exit 1
              fi
              create_callvalue=$1
              shift
              ;;
          --debug-create)
              shift
              create_debug=true
              ;;
        *)
            echo "Error: Unknown option: $1"
            exit 1
            ;;
     esac
 done

# Check for conflicting flags
if [[ -n "$runtime_calldata_sig" ]] && [[ -n "$runtime_raw_calldata" ]]; then
    echo "Error: Cannot use both --runtime-calldata and --runtime-raw-calldata"
    exit 1
fi

if [[ -n "$create_arguments_sig" ]] && [[ -n "$create_raw_args" ]]; then
    echo "Error: Cannot use both --create-arguments and --create-raw-arguments"
    exit 1
fi

# Execute compilation pipeline
echo "Compiling to hull..."
if ! cabal run sol-core -- -f "$file"; then
    echo "Error: sol-core compilation failed"
    exit 1
fi

mkdir -p build
if ls ./output*.hull 1> /dev/null 2>&1; then
    mv ./output*.hull build/
fi

echo "Generating Yul..."
yule_args=("$hull" -o "$yulfile")
if [[ "$create" == "false" ]]; then
    yule_args+=(--nodeploy)
fi
if ! cabal run yule -- "${yule_args[@]}"; then
    echo "Error: yule generation failed"
    exit 1
fi

echo "Compiling to bytecode..."
if ! solc --strict-assembly --bin --optimize "$yulfile" | tail -1 | tr -d '\n' > "$hexfile"; then
    echo "Error: solc compilation failed"
    exit 1
fi

echo "Hex output: $hexfile"

if [[ "$create" == "true" ]]; then
  evm_cmd="evm run --trace --trace.nomemory=false --trace.noreturndata=false"

  # Build and execute evm command for create
  evm_cmd="$evm_cmd --create --dump --codefile $hexfile"

  if [[ -n "$create_arguments_sig" ]]; then
      if ! arguments=$(cast abi-encode "$create_arguments_sig" "${create_args[@]}"); then
          echo "Error: Failed to generate calldata"
          exit 1
      fi
      echo -n "${arguments#0x}" >> $hexfile
  elif [[ -n "$create_raw_args" ]]; then
      echo -n "${create_raw_args#0x}" >> $hexfile
  fi

  if [[ -n "$create_callvalue" ]]; then
      evm_cmd="$evm_cmd --value $create_callvalue"
  fi

  echo "Executing..."
  output=$(eval "$evm_cmd" 2>&1) || true

  # Extract post-state from output and construct proper genesis JSON
  start_line=$(echo "$output" | grep -n '^{' | tail -1 | cut -d: -f1)
  post_state=$(echo "$output" | sed -n "${start_line},\$p" | jq -r '.')

  # Construct proper genesis JSON for evm run --prestate with latest mainnet EVM version
  genesis_json=$(echo "$post_state" | jq '{
    config: {
      chainId: 1,
      homesteadBlock: 0,
      eip150Block: 0,
      eip155Block: 0,
      eip158Block: 0,
      byzantiumBlock: 0,
      constantinopleBlock: 0,
      petersburgBlock: 0,
      istanbulBlock: 0,
      muirGlacierBlock: 0,
      berlinBlock: 0,
      londonBlock: 0,
      arrowGlacierBlock: 0,
      grayGlacierBlock: 0,
      mergeNetsplitBlock: 0,
      shanghaiTime: 0,
      cancunTime: 0,
      pragueTime: 0,
      blobSchedule: {
        cancun: {
          target: 3,
          max: 6,
          baseFeeUpdateFraction: 3338477
        },
        prague: {
          target: 6,
          max: 12,
          baseFeeUpdateFraction: 6676954
        },
        osaka: {
          target: 9,
          max: 18,
          baseFeeUpdateFraction: 10015431
        }
      }
    },
    nonce: 0,
    timestamp: 0,
    extraData: "0x",
    gasLimit: "0x47e7c4",
    difficulty: "0x1",
    mixHash: "0x0000000000000000000000000000000000000000000000000000000000000000",
    coinbase: "0x0000000000000000000000000000000000000000",
    alloc: .accounts
  }')

  echo "$genesis_json" > "$create_poststate"
  echo "$output" | jq -R -c 'fromjson? | select(type == "object")' > "$create_tracefile"

  # Extract the result and error from the create tracefile
  result=$(jq -sr 'last | .output' "$create_tracefile")
  error=$(jq -sr 'last | .error' "$create_tracefile")

  if [[ "$create_debug" == "true" ]]; then
      traceview "$create_tracefile"
  fi

  if [[ "$error" == "null" ]]; then
      echo "Creation successful"
      # echo "returndata: 0x${result}"
  else
      echo "Creation failed: $error"
      echo "returndata: 0x$result"
  fi
fi

evm_cmd="evm run --trace --trace.nomemory=false --trace.noreturndata=false"
# Build and execute evm command for runtime
if [[ "$create" == "true" ]]; then
  evm_cmd="$evm_cmd --receiver 0x1f2a98889594024BFfdA3311CbE69728d392C06D --prestate $create_poststate"
else
  evm_cmd="$evm_cmd --codefile $hexfile"
fi

if [[ -n "$runtime_calldata_sig" ]]; then
    if ! calldata=$(cast calldata "$runtime_calldata_sig" "${runtime_calldata_args[@]}"); then
        echo "Error: Failed to generate calldata"
        exit 1
    fi
    evm_cmd="$evm_cmd --input $calldata"
elif [[ -n "$runtime_raw_calldata" ]]; then
    evm_cmd="$evm_cmd --input $runtime_raw_calldata"
fi

if [[ -n "$runtime_callvalue" ]]; then
    evm_cmd="$evm_cmd --value $runtime_callvalue"
fi

echo "Executing..."
output=$(eval "$evm_cmd" 2>&1) || true
echo "$output" | jq -R -c 'fromjson? | select(type == "object")' > "$runtime_tracefile"

result=$(jq -sr 'last | .output' "$runtime_tracefile")
error=$(jq -sr 'last | .error' "$runtime_tracefile")

if [[ "$runtime_debug" == "true" ]]; then
    traceview "$runtime_tracefile"
fi

if [[ "$error" == "null" ]]; then
    if [[ -n "$runtime_calldata_sig" ]]; then
        echo "Execution successful"
        # Remove quotes from result and decode
        if [[ -n "$result" ]]; then
            decoded=$(cast abi-decode "$runtime_calldata_sig" "0x$result")
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
