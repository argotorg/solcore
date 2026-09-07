import * from std;
import * from std.dispatch;
import {caller} from std.opcodes;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

// Direct tests for the top-level `abi_encode` function (std.sol) across both
// static and dynamic types.
//
// Each method encodes a value with `abi_encode` and returns the resulting
// `memory(bytes)`. The dispatcher then ABI-encodes that `memory(bytes)` return
// as a `bytes` value, so the return data is
//   [0x20 offset][length][ abi_encode output, padded ]
// and the inner `[length][...]` payload is EXACTLY what `abi_encode` produced —
// which is what these tests pin down.
//
// Static types encode inline in the head, with no offset word:
//   * uint256 / bool / address    -> a single 32-byte word (length 0x20)
//   * (uint256, uint256)          -> two head words back to back (length 0x40)
// Dynamic types put an offset word in the head pointing at a tail:
//   * string                      -> [0x20][len][data]      (length 0x60 here)
//   * uint256[]                   -> [0x20][len][elems]      (length 0xa0 here)
contract AbiEncodeTypes {
  constructor() {}

  // --- static ---

  // uint256 is written directly into the head as one word.
  function encUint(x : uint256) public returns (memory<bytes>) {
    return abi_encode(x);
  }

  // bool encodes as a single 0/1 word.
  function encBool(x : bool) public returns (memory<bytes>) {
    return abi_encode(x);
  }

  // address is left-padded into a single word.
  function encAddr(x : address) public returns (memory<bytes>) {
    return abi_encode(x);
  }

  // A fully static tuple has both words in the head, with no offset.
  function encPair(a : uint256, b : uint256) public returns (memory<bytes>) {
    return abi_encode((a, b));
  }

  // --- dynamic ---

  // A string gets a head offset word pointing at a `[len][data]` tail.
  function encStr() public returns (memory<bytes>) {
    let raw : string = "abc";
    let s : memory<string> = Str.fromString(raw);
    return abi_encode(s);
  }

  // A dynamic array gets a head offset word pointing at a `[len][elems]` tail.
  function encArr() public returns (memory<bytes>) {
    let a : memory<DynArray<uint256>> = [11, 22, 33];
    return abi_encode(a);
  }

  // --- tuple ---
  function encTuple() public returns (memory<bytes>) {
    let s: memory<string> = Str.fromString("abc");
    return abi_encode((uint256(123), (s, (true, address(caller())))));
  }
}
