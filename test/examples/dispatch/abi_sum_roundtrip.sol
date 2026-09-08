import * from std;
import * from std.dispatch;
import * from std.Generic;
import * from std.ABIGeneric;

// Roundtrip tests for sum ABI coding: `roundtrip(x) -> x` makes the dispatcher
// DECODE the argument from calldata and then ENCODE it straight back into the
// return data. So the returned bytes must equal the input argument payload
// (the calldata after the 4-byte selector) — i.e. encode ∘ decode = identity.
//
// This pins encode and decode as exact inverses for BOTH:
//   * static sums  (inline [tag][branch], no offset word), and
//   * dynamic sums (offset word in the head, [tag][branch] inline in the tail).
// The discriminant is one flat bytes32 keccak256("Name(argSigs)") variant tag,
// so even a 3-constructor type (D3) carries a single tag word, not a chain.
//
// The dynamic direction is what the sum ABIEncode fix restores: before it,
// encoding a decoded dynamic sum dropped everything but the tag, so the return
// bytes could not match the input.
enum D2 { L(uint256), R(memory<bytes>) }                // dynamic (shallow)
enum D3 { X(uint256), Y(uint256), Z(memory<bytes>) }   // dynamic, 3 constructors
enum S2 { P(uint256), Q(uint256) }                      // static

contract SumRoundtrip {
  constructor() {}

  // dynamic, shallow: decode a D2 then re-encode it.
  function rtD2(x : D2) public returns (D2) {
    return x;
  }

  // dynamic, 3 constructors: the flat keccak tag discriminates all three, so the
  // wire form stays [offset][tag][fields] regardless of constructor position.
  function rtD3(x : D3) public returns (D3) {
    return x;
  }

  // static control: inline layout must round-trip unchanged.
  function rtS2(x : S2) public returns (S2) {
    return x;
  }
}
