// Solidity-style struct through the ABI path: a single-constructor product
// auto-derives Generic + a concrete ABIAttribs + ABIDecode, and encodes through
// the Generic -> (a,b):ABIEncode bridge with NO variant tag — i.e. exactly a
// Solidity tuple `(uint256, uint256)`. This compile-only test pins that a
// struct-typed value can be abi_encode'd and that a struct method parameter /
// return value type-checks and lowers.

import * from std;
import * from std.dispatch;
import * from std.Generic;
import * from std.ABIGeneric;

pragma no-patterson-condition;
pragma no-coverage-condition;
pragma no-bounded-variable-condition;

struct Point {
  x: uint256;
  y: uint256;
}

contract StructAbi {
  constructor() {}

  // struct value -> abi_encode : encodes as the tuple (x, y), two head words.
  function enc(a : uint256, b : uint256) public returns (memory<bytes>) {
    return abi_encode(Point(a, b));
  }

  // struct as method parameter (decoded from calldata) and return value
  // (re-encoded) — decode ∘ encode round-trip through the dispatcher.
  function roundtrip(p : Point) public returns (Point) {
    return p;
  }

  // dot access on a decoded struct parameter.
  function firstField(p : Point) public returns (uint256) {
    return p.x;
  }
}
