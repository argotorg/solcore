#!/usr/bin/env bash

set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$root_dir"

bash ./contest.sh test/examples/dispatch/basic.json
bash ./contest.sh test/examples/dispatch/neg.json
# bash ./contest.sh test/examples/dispatch/miniERC20.json
