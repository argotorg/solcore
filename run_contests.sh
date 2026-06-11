#!/usr/bin/env bash

set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$root_dir"

bash ./contest.sh test/examples/dispatch/basic.json
bash ./contest.sh test/examples/dispatch/neg.json
bash ./contest.sh test/examples/dispatch/context.json
bash ./contest.sh test/examples/dispatch/miniERC20.json
bash ./contest.sh test/examples/dispatch/Revert.json
bash ./contest.sh test/examples/dispatch/ownable.json
bash ./contest.sh test/examples/dispatch/hashes.json
bash ./contest.sh test/examples/dispatch/payable.json
bash ./contest.sh test/examples/dispatch/concat.json
bash ./contest.sh test/examples/dispatch/fallback.json
bash ./contest.sh test/examples/dispatch/ecrecover.json
