import * from std;
import * from std.dispatch;

// Demonstrates the `type(C).publicMethods` primitive together with
// `calculateInterfaceId` from std/dispatch.sol, replicating Solidity's
// `type(I).interfaceId`.
//
// The interface id is the XOR of the selectors of every public method:
//   foo(uint256)   -> 0x2fbebd38
//   bar(address)   -> 0x646ea56d
//   interfaceId()  -> 0xa64d0cd4
//   XOR            -> 0xed9d1481
contract InterfaceId {
  function foo(x : uint256) public returns (uint256) {
    return x;
  }

  function bar(a : address) public returns (uint256) {
    return 0;
  }

  function interfaceId() public returns (bytes4) {
    return calculateInterfaceId(type(InterfaceId).publicMethods);
  }
}
