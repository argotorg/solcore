// Array slices are read-only views: there is no LValueIdxAccess instance for
// arraySlice, so writing through a slice (s[i] = v) must be rejected, matching
// Solidity (slices cannot be assigned to element-wise).
import * from std;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

contract ArraySliceReadOnly {
  function main() returns (uint256) {
    let a : memory<DynArray<uint256>> = [10, 20, 30, 40, 50];
    let s = a[uint256(1):uint256(4)];
    s[uint256(0)] = uint256(99); // must fail: no LValueIdxAccess for arraySlice
    return s[uint256(0)];
  }
}
