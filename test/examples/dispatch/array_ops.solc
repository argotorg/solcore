import {*} from std;
import {*} from std.dispatch;

// Storage-array primitives end to end: push / pop / length / indexed read,
// the two revert paths (index out of range, pop on empty), and the guarantee
// that abandoned slots are zeroed -- so regrowing an array never resurrects the
// values that `pop` or a shrinking `setLength` dropped.
contract ArrayOps {
  xs : uint256[];

  constructor() {}

  // NOTE: not named `add` -- that collides with the Yul builtin of the same name.
  function pushVal(v : uint256) public returns (()) {
    ArrayPush.push(xs, v);
  }

  function popArr() public returns (()) {
    Array.pop(xs);
  }

  function len() public returns (uint256) {
    return Length.length(xs);
  }

  function get(i : uint256) public returns (uint256) {
    return xs[i];
  }

  function grow(n : uint256) public returns (()) {
    Array.setLength(xs, n);
  }
}
