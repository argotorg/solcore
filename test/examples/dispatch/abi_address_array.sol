import * from std;
import * from std.dispatch;
import * from std.Generic;
import * from std.ABIGeneric;

// calldata(array(address)) — a dynamic array of a STATIC value type. Unlike
// bytes[] (dynamic elements, offset table), address is static, so elements sit
// inline at a fixed 32-byte stride (headSize(address) = 32). Each element is a
// left-padded 20-byte address; decoding checks the high 12 bytes are zero
// (DirtyHigherBitsForAddress). Exercises the static-element abiArrayGet branch.
contract AddressArr {
  constructor() {}

  // The i-th address.
  function at(items : calldata<array<address>>, i : uint256) public returns (address) {
    return items[i];
  }

  // Number of elements.
  function count(items : calldata<array<address>>) public returns (uint256) {
    return items.length();
  }
}
