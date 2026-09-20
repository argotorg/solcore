// Array slices x[start:end] over a MEMORY array, using the SURFACE syntax.
//   a[start:end]  -> sliceRange   a[start:] -> sliceFrom
//   a[:end]       -> sliceTo      a[:]      -> sliceAll
// A slice is a read-only view: .length() and s[i] delegate to the base array.
// Slices are re-sliceable and can be materialised back to an array (toArray).
// (Index/value literals are written uint256(..) to pin their type, since a bare
//  integer literal index is otherwise ambiguous - a pre-existing limitation.)
import * from std;
import * from std.dispatch;

contract ArraySlice {
  // build [10, 11, 12, 13, 14]
  function build() returns (uint256[]) {
    let a : uint256[] = new uint256[](5);
    a[uint256(0)] = uint256(10);
    a[uint256(1)] = uint256(11);
    a[uint256(2)] = uint256(12);
    a[uint256(3)] = uint256(13);
    a[uint256(4)] = uint256(14);
    return a;
  }

  // a[1:4].length() == 3
  function rangeLen() public returns (uint256) {
    return build()[uint256(1):uint256(4)].length();
  }

  // a[1:4][i]: i in {0,1,2} -> {11,12,13}; i >= 3 -> revert OutOfBounds
  function rangeGet(i : uint256) public returns (uint256) {
    return build()[uint256(1):uint256(4)][i];
  }

  // a[2:].length() == 3
  function fromLen() public returns (uint256) {
    return build()[uint256(2):].length();
  }

  // a[:3].length() == 3
  function toLen() public returns (uint256) {
    return build()[:uint256(3)].length();
  }

  // a[:].length() == 5
  function allLen() public returns (uint256) {
    return build()[:].length();
  }

  // re-slice: a[1:4] = {11,12,13}; [1:] = {12,13}; index i
  function reslice(i : uint256) public returns (uint256) {
    return build()[uint256(1):uint256(4)][uint256(1):][i];
  }

  // construction bound: start > end -> revert OutOfBounds
  function badStartEnd() public returns (uint256) {
    return build()[uint256(4):uint256(2)].length();
  }

  // construction bound: end > length -> revert OutOfBounds
  function badEnd() public returns (uint256) {
    return build()[uint256(0):uint256(6)].length();
  }

  // materialise a[1:4] into a fresh array, then index it
  function materialise(i : uint256) public returns (uint256) {
    let b : uint256[] = toArray(build()[uint256(1):uint256(4)]);
    return b[i];
  }
}
