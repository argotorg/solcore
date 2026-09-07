// `arr[i]` on a *local* storage-array reference, not a contract field.
// The local already holds the storage reference, so the desugaring emits
// `ridx(arr, i)` / `lidx(arr, i)` directly (cf. 129arraystorage.solc, which
// had to spell out `ridx` by hand).
import {*} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

contract LocalIndex {
  reserved : word; // forge uses at least 1 storage slot

  function main() returns (uint256) {
    let arr : uint256[] storage = storage(0x100);

    ArrayPush.push(arr, uint256(42));
    ArrayPush.push(arr, uint256(100));

    // Indexed read through a local.
    let sum : uint256 = arr[uint256(0)] + arr[uint256(1)];

    // Indexed write through a local.
    arr[uint256(0)] = uint256(1);

    return sum + arr[uint256(0)];
  }
}
