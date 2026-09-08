import * from std hiding {storeArrayLit};
import * from std.dispatch;

// Source declarations must not capture the helper used by storage literals.
function storeArrayLit() returns (word) { return 99; }

// Array literals, end to end.
//
// `[e1,...,en]` builds a memory array. Assigning one to a storage array field is
// Solidity's memory -> storage copy: it resizes the field and clears the
// abandoned tail, so shrinking must not leave old elements reachable.
contract ArrayLit {
  xs : array<uint256>;

  constructor() {}

  // --- memory literals ---

  // Reads back an element of a memory literal. Element 0 must be the first
  // element, not the length word stored ahead of it.
  function memAt(i : uint256) public returns (uint256) {
    let m : memory<DynArray<uint256>> = [11, 22, 33];
    return m[i];
  }

  function memSum() public returns (uint256) {
    let m : memory<DynArray<uint256>> = [1, 2, 3, 4];
    let acc : uint256 = uint256(0);
    let i : uint256;
    for (i = uint256(0); i < uint256(4); i = i + uint256(1)) {
      acc = acc + m[i];
    }
    return acc;
  }

  // Nested literal: the element type is itself a memory array.
  function nested() public returns (uint256) {
    let g : memory<DynArray<memory<DynArray<uint256>>>> = [[1, 2], [3, 4]];
    let row : memory<DynArray<uint256>> = g[uint256(1)];
    return row[uint256(0)];
  }

  // --- storage literals ---

  function setThree() public returns (()) {
    xs = [10, 20, 30];
  }

  function setFive() public returns (()) {
    xs = [1, 2, 3, 4, 5];
  }

  function setTwo() public returns (()) {
    xs = [7, 8];
  }

  function clear() public returns (()) {
    xs = [];
  }

  function len() public returns (uint256) {
    return Length.length(xs);
  }

  function get(i : uint256) public returns (uint256) {
    return xs[i];
  }

  // Grow the array back without writing elements. Anything the shrink abandoned
  // must read as zero, not as the old value.
  function grow(n : uint256) public returns (()) {
    Array.setLength(xs, n);
  }
}
