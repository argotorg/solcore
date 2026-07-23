import {*} from std;
import {mstore} from std.opcodes;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

// Regression for the `|` ambiguity between the bitwise-or operator and the
// match-arm separator. Each arm below ends in a *bare* expression statement
// (no trailing `;`), which is exactly the shape that previously made the
// parser read `mstore(...) | <next-pattern> => ...` as a single bitwise-or
// expression and break the `match`. The `|` *inside* the parentheses is a
// genuine bitwise-or; the `|` that starts each arm is a separator.
function emit(x: word) returns (()) {
  match (x ) {
    case 0 { mstore(0, x | 1);
    } case 1 { mstore(0, x & 1);
    } default { mstore(0, x);
  } }
}

contract MatchBitwise {
  // `0 | 1` still folds to 1 at the top level.
  function main() public returns (word) {
    emit(0);
    return 0 | 1;
  }
}
