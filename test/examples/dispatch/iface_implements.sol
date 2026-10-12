// `implements` conformance: a contract that provides every interface method
// type-checks. Interfaces are also callable (see iface_selfcall.sol).
import * from std;
import * from std.dispatch;

interface IMinimal {
  function ping(x : uint256) returns (uint256);
}

contract Service implements IMinimal {
  constructor() {}
  function ping(x : uint256) public returns (uint256) {
    return x;
  }
}
