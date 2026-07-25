// Storage array as a contract field: `arr : array(uint256)`.
import {*} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

contract ArrayField {
  reserved : word; // forge uses at least 1 storage slot
  arr : uint256[];

  function main() returns (uint256) {
    // push appends and grows the length automatically.
    ArrayPush.push(arr, uint256(42));
    ArrayPush.push(arr, uint256(100));

    // arr[i] — bounds-checked indexed access.
    return arr[uint256(0)] + arr[uint256(1)];
  }
}
