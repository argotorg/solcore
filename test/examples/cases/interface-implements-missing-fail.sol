// `implements` conformance: a contract that claims to implement an interface but
// omits one of its methods is rejected at compile time.
import * from std;
import * from std.dispatch;

interface IERC20 {
  function transfer(to : address, amount : uint256) returns (bool);
  function transferFrom(from : address, to : address, amount : uint256) returns (bool);
}

contract Token implements IERC20 {
  function transfer(to : address, amount : uint256) public returns (bool) { return true; }
  // transferFrom is missing -> conformance error
}
