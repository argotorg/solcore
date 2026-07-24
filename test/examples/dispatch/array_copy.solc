import std.{*};
import std.dispatch.{*};

// Whole-array assignment `a = b` follows Solidity: it is a deep copy, not an
// alias; assigning an array to itself is a no-op; and a copy that shrinks the
// destination clears the slots it abandons, so regrowing yields zeros.
contract ArrayCopy {
  a : array(uint256);
  b : array(uint256);

  constructor() {}

  public function pushA(v : uint256) -> () {
    ArrayPush.push(a, v);
  }

  public function pushB(v : uint256) -> () {
    ArrayPush.push(b, v);
  }

  // a = b
  public function copyBintoA() -> () {
    a = b;
  }

  // a = a  (must be a no-op, not a self-clobbering copy)
  public function copyAintoA() -> () {
    a = a;
  }

  public function setB(i : uint256, v : uint256) -> () {
    b[i] = v;
  }

  public function growA(n : uint256) -> () {
    Array.setLength(a, n);
  }

  public function lenA() -> uint256 {
    return Length.length(a);
  }

  public function getA(i : uint256) -> uint256 {
    return a[i];
  }
}
