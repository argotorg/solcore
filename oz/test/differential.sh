#!/usr/bin/env bash
# Differential test of the ERC20 translation. THREE bytecodes are driven by the
# SAME ordered call sequence with expected returndata/status (oz/ERC20Inherit.json):
#
#   1. oz/ERC20Flat.solc     — Core Solidity, NO inheritance (one contract)
#   2. oz/ERC20Inherit.solc  — Core Solidity, real hierarchy (Token inherits
#                              ERC20Pausable, ERC20Capped; C3 super chain)
#   3. oz/test/OZToken.sol    — Classic Solidity reference (ERC20 + Pausable + Capped)
#
# All three passing on the identical spec proves they produce byte-identical
# execution results — i.e. the translation (with AND without inheritance)
# reproduces solc's semantics, and the inheritance machinery preserves them.
#
# Requires solc, foundry (cast), the built testrunner, and evmone. OZ sources come
# from $OZ_ROOT (a checkout of openzeppelin-contracts) or a shallow clone.
set -uo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$root_dir"
spec="oz/ERC20Inherit.json"

testrunner_exe="${testrunner_exe:-build/test/testrunner/testrunner}"
[[ -x "$testrunner_exe" ]] || { echo "FAIL: testrunner not built ($testrunner_exe). Run: cmake --build build --target testrunner"; exit 1; }
evmone_lib=""
for c in "${evmone:-}" "$HOME/.local/lib/libevmone.so" "$root_dir/result/lib/libevmone.so" /nix/store/*/lib/libevmone.so; do
  [[ -n "$c" && -f "$c" ]] && evmone_lib="$c" && break
done
[[ -n "$evmone_lib" ]] || { echo "FAIL: libevmone.so not found"; exit 1; }

work="$(mktemp -d)"; trap 'rm -rf "$work"' EXIT

# OZ sources are vendored under oz/vendor (the minimal transitive closure that
# OZToken.sol needs), so the test is hermetic — no network / git checkout. Point
# OZ_ROOT at a full openzeppelin-contracts checkout to use that instead; a clone
# is only a last-resort fallback if the vendored tree is missing.
OZ_ROOT="${OZ_ROOT:-$root_dir/oz/vendor}"
if [[ ! -f "$OZ_ROOT/contracts/token/ERC20/ERC20.sol" ]]; then
  echo "==> OZ sources not found at $OZ_ROOT; cloning openzeppelin-contracts (shallow) ..."
  git clone --depth 1 https://github.com/OpenZeppelin/openzeppelin-contracts.git "$work/oz" >/dev/null 2>&1 \
    || { echo "FAIL: OZ sources missing and clone failed (expected oz/vendor or set OZ_ROOT)"; exit 1; }
  OZ_ROOT="$work/oz"
fi

SOLCORE_CMD="${SOLCORE_CMD:-cabal exec sol-core --}"
YULE_CMD="${YULE_CMD:-cabal run exe:yule --}"

compile_oz () { # -> stdout: creation bytecode
  solc --bin --optimize "contracts/=$OZ_ROOT/contracts/" oz/test/OZToken.sol 2>/dev/null \
    | awk '/OZToken.sol:Token/{f=1} f&&/^[0-9a-f]{200,}$/{print; exit}'
}
compile_core () { # <src.solc> -> stdout: creation bytecode
  local src="$1" d; d="$work/$(basename "$src" .solc)"
  mkdir -p "$d"
  $SOLCORE_CMD -f "$src" -o "$d" >/dev/null 2>&1 || return 1
  local hull; hull="$(ls "$d"/output*.hull 2>/dev/null | head -1)"
  [[ -n "$hull" ]] || return 1
  $YULE_CMD "$hull" -o "$d/out.yul" >/dev/null 2>&1 || return 1
  solc --strict-assembly --bin --optimize "$d/out.yul" 2>/dev/null | tail -1
}

run_side () { # <label> <bytecode>
  local label="$1" bin="$2"
  [[ -n "$bin" ]] || { echo "  [$label] FAIL (empty bytecode)"; return 1; }
  python3 - "$spec" "$bin" "$work/$label.in.json" <<'PY'
import json,sys
d=json.load(open(sys.argv[1])); d[next(iter(d))]["bytecode"]=sys.argv[2]
json.dump(d,open(sys.argv[3],"w"))
PY
  if "$testrunner_exe" "$evmone_lib" "$work/$label.in.json" "$work/$label.out.json" >"$work/$label.log" 2>&1; then
    echo "  [$label] PASS ($(grep -oE '[0-9]+ tests performed' "$work/$label.log"))"; return 0
  else
    echo "  [$label] FAIL"; grep -iE "Expected|Creation|Abort" "$work/$label.log" | head; return 1
  fi
}

nvec="$(python3 -c "import json;d=json.load(open('$spec'));print(len(d[next(iter(d))]['tests']))")"
echo "==> compiling OZ reference (solc) ..."; oz_bin="$(compile_oz)"
echo "==> compiling Core no-inheritance (oz/ERC20Flat.solc) ..."; flat_bin="$(compile_core oz/ERC20Flat.solc)"
echo "==> compiling Core inheritance (oz/ERC20Inherit.solc) ..."; inh_bin="$(compile_core oz/ERC20Inherit.solc)"

echo "==> differential over $nvec vectors:"
rc=0
run_side oz-classic-solidity "$oz_bin"   || rc=1
run_side core-no-inheritance "$flat_bin" || rc=1
run_side core-inheritance    "$inh_bin"  || rc=1

if [[ $rc -eq 0 ]]; then
  echo "==> DIFFERENTIAL PASS: Core (flat) == Core (inheritance) == Classic Solidity, byte-for-byte"
else
  echo "==> DIFFERENTIAL FAIL"
fi
exit $rc
