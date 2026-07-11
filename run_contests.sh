#!/usr/bin/env bash

set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$root_dir"

bash ./contest.sh test/examples/dispatch/basic.json
bash ./contest.sh test/examples/dispatch/assembly.json
bash ./contest.sh test/examples/dispatch/neg.json
bash ./contest.sh test/examples/dispatch/miniERC20.json
bash ./contest.sh test/examples/dispatch/Revert.json
bash ./contest.sh test/examples/dispatch/ownable.json
bash ./contest.sh test/examples/dispatch/hashes.json
bash ./contest.sh test/examples/dispatch/payable.json
bash ./contest.sh test/examples/dispatch/payable_ctor.json
bash ./contest.sh test/examples/dispatch/nonpayable_ctor.json
bash ./contest.sh test/examples/dispatch/concat.json
bash ./contest.sh test/examples/dispatch/slices.json
bash ./contest.sh test/examples/dispatch/fallback.json
bash ./contest.sh test/examples/dispatch/ecrecover.json
bash ./contest.sh test/examples/dispatch/memory.json
bash ./contest.sh test/examples/dispatch/storage.json
bash ./contest.sh test/examples/dispatch/transient_field.json
bash ./contest.sh test/examples/dispatch/generic_sum.json
bash ./contest.sh test/examples/dispatch/generic_product.json
bash ./contest.sh test/examples/dispatch/sum_wide_product.json
bash ./contest.sh test/examples/dispatch/specialise_sum_of_product.json
bash ./contest.sh test/examples/dispatch/storage_adt_field.json
bash ./contest.sh test/examples/dispatch/storage_adt_enum.json
bash ./contest.sh test/examples/dispatch/storage_adt_bool.json
bash ./contest.sh test/examples/dispatch/storage_adt_mapping.json
bash ./contest.sh test/examples/dispatch/storage_adt_abi.json
bash ./contest.sh test/examples/dispatch/storage_dynamic_field.json
bash ./contest.sh test/examples/dispatch/forloops.json
bash ./contest.sh test/examples/dispatch/weth9.json
