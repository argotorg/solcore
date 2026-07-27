import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
import std.ABIGeneric.{*};

// calldata(array(address)) — a dynamic array of a STATIC value type. Unlike
// bytes[] (dynamic elements, offset table), address is static, so elements sit
// inline at a fixed 32-byte stride (headSize(address) = 32). Each element is a
// left-padded 20-byte address; decoding checks the high 12 bytes are zero
// (DirtyHigherBitsForAddress). Exercises the static-element abiArrayGet branch.
contract AddressArr {
  constructor() {}

  // The i-th address.
  public function at(items : calldata(array(address)), i : uint256) -> address {
    return items[i];
  }

  // Number of elements.
  public function count(items : calldata(array(address))) -> uint256 {
    return items.length();
  }
}
