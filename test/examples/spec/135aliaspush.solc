// Binding a storage array field to a local is an *alias*, not a copy: the local
// holds the same slot, so growing it grows the field. (Solidity's `T[] storage p`.)
import {*} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

contract AliasPush {
  reserved : word; // forge uses at least 1 storage slot
  xs : uint256[];

  function main() returns (uint256) {
    let p : uint256[] storage = xs;
    ArrayPush.push(p, uint256(1));
    // The push went through the alias, so the field sees it.
    return Length.length(xs);
  }
}
