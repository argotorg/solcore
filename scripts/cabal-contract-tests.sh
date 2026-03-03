#!/usr/bin/env bash

set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root_dir"

allow_skip="${SOLCORE_CONTRACT_TESTS_ALLOW_SKIP:-0}"

fail_or_maybe_skip() {
    local message="$1"
    if [[ "$allow_skip" != "1" ]]; then
        echo "[sol-core-contract-test] $message" >&2
        exit 1
    fi

    echo "[sol-core-contract-test] Skipping contract tests: $message" >&2
    echo "[sol-core-contract-test] Set SOLCORE_CONTRACT_TESTS_ALLOW_SKIP=0 (or unset it) to fail instead." >&2
    exit 0
}

ensure_testrunner() {
    if [[ -n "${testrunner_exe:-}" ]]; then
        if [[ -x "$testrunner_exe" ]]; then
            return
        fi
        fail_or_maybe_skip "testrunner_exe is set but not executable: $testrunner_exe"
    fi

    local candidates=(
        "$root_dir/build/test/testrunner/testrunner"
        "$root_dir/test/testrunner/testrunner"
    )

    for candidate in "${candidates[@]}"; do
        if [[ -x "$candidate" ]]; then
            testrunner_exe="$candidate"
            export testrunner_exe
            return
        fi
    done

    if ! command -v cmake >/dev/null 2>&1; then
        fail_or_maybe_skip "cmake is required to build testrunner."
    fi

    echo "[sol-core-contract-test] Building testrunner..."
    local cmake_configure_args=(
        -S "$root_dir"
        -B "$root_dir/build"
    )

    if [[ ! -f "$root_dir/deps/nlohmann_json/CMakeLists.txt" ]] && command -v git >/dev/null 2>&1; then
        git -C "$root_dir" submodule update --init --depth 1 deps/nlohmann_json >/dev/null 2>&1 || true
    fi

    if [[ ! -f "$root_dir/deps/nlohmann_json/CMakeLists.txt" ]]; then
        cmake_configure_args+=("-DIGNORE_VENDORED_DEPENDENCIES=ON")
    fi

    if ! cmake "${cmake_configure_args[@]}"; then
        fail_or_maybe_skip "failed to configure testrunner build (missing deps/nlohmann_json or system nlohmann_json package)."
    fi

    if ! cmake --build "$root_dir/build" --target testrunner; then
        fail_or_maybe_skip "failed to build testrunner."
    fi

    if [[ -x "$root_dir/build/test/testrunner/testrunner" ]]; then
        testrunner_exe="$root_dir/build/test/testrunner/testrunner"
        export testrunner_exe
        return
    fi

    fail_or_maybe_skip "testrunner build finished but binary was not found."
}

ensure_evmone() {
    if [[ -n "${evmone:-}" ]]; then
        if [[ -f "$evmone" ]]; then
            return
        fi
        fail_or_maybe_skip "evmone is set but the file does not exist: $evmone"
    fi

    local candidates=(
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

    for candidate in "${candidates[@]}"; do
        if [[ -f "$candidate" ]]; then
            evmone="$candidate"
            export evmone
            return
        fi
    done

    if command -v brew >/dev/null 2>&1; then
        local brew_prefix
        brew_prefix="$(brew --prefix 2>/dev/null || true)"
        if [[ -n "$brew_prefix" && -f "$brew_prefix/lib/libevmone.dylib" ]]; then
            evmone="$brew_prefix/lib/libevmone.dylib"
            export evmone
            return
        fi
    fi

    fail_or_maybe_skip "libevmone not found. Set evmone=/absolute/path/to/libevmone.{so,dylib}."
}

ensure_testrunner
ensure_evmone

echo "[sol-core-contract-test] Using testrunner: $testrunner_exe"
echo "[sol-core-contract-test] Using evmone: $evmone"

bash ./run_contests.sh
