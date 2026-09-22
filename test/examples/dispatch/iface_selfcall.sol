// Typed external call via an `interface`, exercised as a self-call (single
// contract, so it runs under the standard dispatch harness). `Doubler(self())`
// builds a typed handle to this contract's own address; `.double(x)` is a typed
// cross-contract call that desugars to selector + abi_encode + raw_call and
// re-enters the contract through its ABI dispatcher.
import * from std;
import * from std.dispatch;
import {address as address_} from std.opcodes;

function self() returns (address) {
  return address(address_());
}

interface Doubler {
  function double(x : uint256) returns (uint256);
}

contract C {
  constructor() {}

  function double(x : uint256) public returns (uint256) {
    return x + x;
  }

  // Typed external self-call: Doubler(self()).double(x) == double(x).
  function callDouble(x : uint256) public returns (uint256) {
    return Doubler(self()).double(x);
  }
}
