import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
import std.ABIGeneric.{*};

// Roundtrip tests for sum ABI coding: `roundtrip(x) -> x` makes the dispatcher
// DECODE the argument from calldata and then ENCODE it straight back into the
// return data. So the returned bytes must equal the input argument payload
// (the calldata after the 4-byte selector) — i.e. encode ∘ decode = identity.
//
// This pins encode and decode as exact inverses for BOTH:
//   * static sums  (inline [tag][branch], no offset word), and
//   * dynamic sums (offset word in the head, [tag][branch] inline in the tail,
//     one offset word per nested dynamic level).
//
// The dynamic direction is what the sum(f,g):ABIEncode fix restores: before it,
// encoding a decoded dynamic sum dropped everything but the tag, so the return
// bytes could not match the input.
data D2 = L(uint256) | R(memory(bytes));                // dynamic (shallow)
data D3 = X(uint256) | Y(uint256) | Z(memory(bytes));   // dynamic (deeply nested)
data S2 = P(uint256) | Q(uint256);                      // static

contract SumRoundtrip {
  constructor() {}

  // dynamic, shallow: decode a sum(uint256, bytes) then re-encode it.
  public function rtD2(x : D2) -> D2 {
    return x;
  }

  // dynamic, deeply right-nested: each nested dynamic level round-trips its own
  // offset word.
  public function rtD3(x : D3) -> D3 {
    return x;
  }

  // static control: inline layout must round-trip unchanged.
  public function rtS2(x : S2) -> S2 {
    return x;
  }
}
