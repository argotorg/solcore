// Memory dynamic arrays à la Solidity, using the SURFACE syntax:
//   uint256[]         (type)       desugars to memory<DynArray<uint256>>
//   new uint256[](n)  (expression) desugars to newArrayP(@uint256, n)
// plus .length, index read m[i], and index write m[i] = v.
import * from std;
import * from std.dispatch;

contract MemArray {
  // new uint256[](n) then read its length
  function newLen(n : uint256) public returns (uint256) {
    let m : uint256[] = new uint256[](n);
    return m.length();
  }

  // create, write m[i] = v, read back m[i]
  function setGet(n : uint256, i : uint256, v : uint256) public returns (uint256) {
    let m : uint256[] = new uint256[](n);
    m[i] = v;
    return m[i];
  }

  // zero-initialised: an unwritten slot reads as 0
  function defaultZero(n : uint256, i : uint256) public returns (uint256) {
    let m : uint256[] = new uint256[](n);
    return m[i];
  }
}
