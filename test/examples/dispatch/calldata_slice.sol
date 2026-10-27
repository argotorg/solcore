// Array slices over a CALLDATA array, proving slices are location-independent:
// the very same a[start:end] syntax and read-only view work over a lazily
// decoded calldata<array<uint256>> handle, delegating to the calldata Length /
// RValueIdxAccess instances. A slice index past the view length reverts with the
// generic OutOfBounds selector (b4120f14), checked before the base decode.
import * from std;
import * from std.dispatch;

contract CalldataSlice {
  // xs[1:4].length() == 3
  function sliceLen(xs : calldata<array<uint256>>) public returns (uint256) {
    return xs[uint256(1):uint256(4)].length();
  }

  // xs[1:4][i]: i in {0,1,2} -> {xs[1],xs[2],xs[3]}; i >= 3 -> revert
  function sliceGet(xs : calldata<array<uint256>>, i : uint256) public returns (uint256) {
    return xs[uint256(1):uint256(4)][i];
  }

  // xs[:].length() == xs.length()
  function sliceAllLen(xs : calldata<array<uint256>>) public returns (uint256) {
    return xs[:].length();
  }
}
