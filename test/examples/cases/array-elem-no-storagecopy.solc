// An element type with no `StorageCopy` instance cannot be the element of a
// storage array: `CanStore` for `storage(array(t))` -- which every field access
// goes through -- requires `t:StorageCopy`. Rejecting this at compile time is
// what keeps `a = b` from silently shallow-copying a type it cannot copy.
import {*} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

enum Odd { Odd(word) }

contract NoCopy {
  reserved : word;
  xs : Odd[];

  function main() returns (uint256) {
    return Length.length(xs);
  }
}
