import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
import std.ABIGeneric.{*};

// calldata(array(bytes)) — a dynamic array whose element is itself dynamic, the
// canonical Solidity `bytes[]`. After the length word the region is a table of
// 32-byte offsets (relative to the region base), one per element, each pointing
// at that element's `[length][data]` encoding. `items[i]` decodes the i-th
// element on demand: abiArrayGet hands the element decoder the region base +
// element i's slot, and the memory(bytes) decoder follows that offset to the
// element's length word — no ADT wrapper needed, unlike abi_batch_adt.
contract BytesArray {
  constructor() {}

  // The i-th bytes element.
  public function at(items : calldata(array(memory(bytes))), i : uint256) -> memory(bytes) {
    return items[i];
  }

  // Number of elements.
  public function count(items : calldata(array(memory(bytes)))) -> uint256 {
    return items.length();
  }
}
