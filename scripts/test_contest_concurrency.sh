#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp_parent="${TMPDIR:-/tmp}"
tmp_parent="${tmp_parent%/}"
test_root="$(mktemp -d "$tmp_parent/solcore-contest-concurrency.XXXXXX")"
test_marker="$test_root/.owned-by-contest-concurrency-test"
touch "$test_marker"

cleanup_test_root() {
    if [[ "$test_root" != "$tmp_parent"/solcore-contest-concurrency.* || ! -f "$test_marker" ]]; then
        echo "Error: refusing to clean unverified test directory '$test_root'" >&2
        return 1
    fi

    rm -rf -- "$test_root"
}

trap cleanup_test_root EXIT
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

mkdir -p \
    "$test_root/cases/alpha" \
    "$test_root/cases/beta" \
    "$test_root/fake-bin" \
    "$test_root/state"
cp "$repo_root/contest.sh" "$test_root/contest.sh"
chmod +x "$test_root/contest.sh"

printf '%s\n' alpha > "$test_root/cases/alpha/shared.solc"
printf '%s\n' '{"shared": {}}' > "$test_root/cases/alpha/shared.json"
printf '%s\n' beta > "$test_root/cases/beta/shared.solc"
printf '%s\n' '{"shared": {}}' > "$test_root/cases/beta/shared.json"
printf '%s\n' user-owned-sentinel > "$test_root/output1.hull"
touch "$test_root/libevmone.so"

cat > "$test_root/fake-bin/tool" <<'EOF'
#!/usr/bin/env bash

set -euo pipefail

tool="$(basename "$0")"

case "$tool" in
    sol-core)
        src=
        output_dir=
        while [[ $# -gt 0 ]]; do
            case "$1" in
                -f)
                    src="$2"
                    shift 2
                    ;;
                -o)
                    output_dir="$2"
                    shift 2
                    ;;
                *)
                    shift
                    ;;
            esac
        done

        case_name="$FAKE_CASE"
        [[ "$(<"$src")" == "$case_name" ]]
        printf '%s\n' "$case_name" > "$output_dir/output1.hull"
        printf '%s\n' "$output_dir" > "$FAKE_STATE/$case_name.work-dir"
        touch "$FAKE_STATE/compiler-$case_name.ready"

        deadline=$((SECONDS + 10))
        until [[ -f "$FAKE_STATE/compiler-alpha.ready" && -f "$FAKE_STATE/compiler-beta.ready" ]]; do
            if (( SECONDS >= deadline )); then
                echo "Timed out waiting for both fake compilers" >&2
                exit 1
            fi
            sleep 0.01
        done
        ;;
    yule)
        hull="$1"
        shift
        output=
        while [[ $# -gt 0 ]]; do
            case "$1" in
                -o)
                    output="$2"
                    shift 2
                    ;;
                *)
                    shift
                    ;;
            esac
        done

        case_name="$(<"$hull")"
        printf '%s\n' "$output" > "$FAKE_STATE/$case_name.yul-path"
        printf '%s\n' "$case_name" > "$output"
        ;;
    solc)
        yul=
        for arg in "$@"; do
            yul="$arg"
        done
        case_name="$(<"$yul")"
        [[ "$case_name" == "$FAKE_CASE" ]]
        printf '%s\n' "$yul" > "$FAKE_STATE/$case_name.solc-input"
        printf '%s\n' "Binary representation:" "hex-$case_name"
        ;;
    jq)
        if [[ "$1" == "keys[0]" ]]; then
            printf '%s\n' '"shared"'
        else
            [[ "$1" == *"hex-$FAKE_CASE"* ]]
            printf '{"shared":{"bytecode":"%s","case":"%s"}}\n' \
                "hex-$FAKE_CASE" "$FAKE_CASE"
        fi
        ;;
    testrunner)
        [[ -f "$1" ]]
        [[ -f "$2" ]]
        command grep -q "\"case\":\"$FAKE_CASE\"" "$2"
        printf '%s\n' "$2" > "$FAKE_STATE/$FAKE_CASE.runner-input"
        printf '%s\n' "$3" > "$FAKE_STATE/$FAKE_CASE.runner-output"
        printf '{"ok":true,"case":"%s"}\n' "$FAKE_CASE" > "$3"
        ;;
    *)
        echo "Unexpected fake tool name: $tool" >&2
        exit 1
        ;;
esac
EOF

chmod +x "$test_root/fake-bin/tool"
for tool in sol-core yule solc jq testrunner; do
    ln -s tool "$test_root/fake-bin/$tool"
done

run_case() {
    local case_name="$1"

    PATH="$test_root/fake-bin:$PATH" \
        SOLCORE_CMD="$test_root/fake-bin/sol-core" \
        YULE_CMD="$test_root/fake-bin/yule" \
        testrunner_exe="$test_root/fake-bin/testrunner" \
        evmone="$test_root/libevmone.so" \
        FAKE_STATE="$test_root/state" \
        FAKE_CASE="$case_name" \
        bash "$test_root/contest.sh" \
        "$test_root/cases/$case_name/shared.json" \
        > "$test_root/state/$case_name.log" 2>&1
}

run_case alpha &
alpha_pid=$!
run_case beta &
beta_pid=$!

failed=0
if ! wait "$alpha_pid"; then
    command cat "$test_root/state/alpha.log" >&2
    failed=1
fi
if ! wait "$beta_pid"; then
    command cat "$test_root/state/beta.log" >&2
    failed=1
fi
if [[ "$failed" != "0" ]]; then
    exit 1
fi

alpha_work_dir="$(<"$test_root/state/alpha.work-dir")"
beta_work_dir="$(<"$test_root/state/beta.work-dir")"
alpha_yul_path="$(<"$test_root/state/alpha.yul-path")"
beta_yul_path="$(<"$test_root/state/beta.yul-path")"
alpha_solc_input="$(<"$test_root/state/alpha.solc-input")"
beta_solc_input="$(<"$test_root/state/beta.solc-input")"
alpha_runner_input="$(<"$test_root/state/alpha.runner-input")"
beta_runner_input="$(<"$test_root/state/beta.runner-input")"
alpha_runner_output="$(<"$test_root/state/alpha.runner-output")"
beta_runner_output="$(<"$test_root/state/beta.runner-output")"

[[ "$alpha_work_dir" == "$test_root/build/.contest-work/run."* ]]
[[ "$beta_work_dir" == "$test_root/build/.contest-work/run."* ]]
[[ "$alpha_work_dir" != "$beta_work_dir" ]]
[[ "$alpha_yul_path" == "$alpha_work_dir/output.yul" ]]
[[ "$beta_yul_path" == "$beta_work_dir/output.yul" ]]
[[ "$alpha_yul_path" != "$beta_yul_path" ]]
[[ "$alpha_solc_input" == "$alpha_yul_path" ]]
[[ "$beta_solc_input" == "$beta_yul_path" ]]
[[ "$alpha_runner_input" == "$alpha_work_dir/runner-input.json" ]]
[[ "$beta_runner_input" == "$beta_work_dir/runner-input.json" ]]
[[ "$alpha_runner_input" != "$beta_runner_input" ]]
[[ "$alpha_runner_output" == "$alpha_work_dir/runner-output.json" ]]
[[ "$beta_runner_output" == "$beta_work_dir/runner-output.json" ]]
[[ "$alpha_runner_output" != "$beta_runner_output" ]]
[[ ! -e "$alpha_work_dir" ]]
[[ ! -e "$beta_work_dir" ]]
[[ "$(<"$test_root/output1.hull")" == "user-owned-sentinel" ]]
[[ ! -e "$test_root/build/shared.yul" ]]
[[ ! -e "$test_root/build/shared.hex" ]]
[[ ! -e "$test_root/build/shared.json" ]]
[[ ! -e "$test_root/build/shared-output.json" ]]

printf '%s\n' "contest concurrency regression passed"
