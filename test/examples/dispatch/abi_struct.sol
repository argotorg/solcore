import * from std;
import * from std.dispatch;
import * from std.Generic;
import * from std.ABIGeneric;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

// End-to-end ABI dispatch test for Solidity-style structs, including a nested
// ADT field.
//
// `Point` is a single-constructor product whose third field `color` is an ADT
// (`Option(Color)`). Every branch of `Option(Color)` is static, so the sum is
// static and `Point` stays a fully static value: it occupies 8 head words
// (0x100 bytes) with no tail. Over the wire it is the tuple
// (uint256, uint256, Option(Color)); the ADT field is encoded inline as a
// variant-tagged sum — a bytes32 keccak256("Ctor(argSigs)") discriminant
// followed by the branch payload, zero-padded to the widest branch.
//
// The test drives both ABI directions through the dispatcher:
//   * ENCODE          — `enc` builds a struct and abi_encode's it.
//   * DECODE + ENCODE  — `roundtrip` takes a struct parameter<decoded from
//                        calldata> and returns it (re-encoded).
//   * DECODE + PROJECT — `getX` decodes a struct and returns a field read via
//                        dot notation; `swap` reads all three fields and
//                        rebuilds a struct, carrying the ADT `color` through.
//
// Because a struct param's signature flattens to its field types, the selector
// of `roundtrip(Point)` is
//   keccak256("roundtrip(uint256,uint256,sum(,sum(,sum(,uint256,uint256,uint256))))")
// — the color field contributing its structural sum signature.

enum Color { White, Black, RGB(uint256, uint256, uint256) }

enum Option<a> { None, Some(a) }

struct Point {
  x: uint256;
  y: uint256;
  color: Option<Color>;
}

contract StructDispatch {
  constructor() {}

  // ENCODE: struct value -> abi_encode -> memory(bytes), returned as `bytes`:
  //   [0x20 offset][len 0x100][x][y][None tag][zero padding to widest branch]
  // Point(a, b, None): the color sum is inline as its None variant tag followed
  // by zero padding sized to the widest (RGB) branch.
  function enc(a : uint256, b : uint256) public returns (memory<bytes>) {
    return abi_encode(Point(a, b, Option.None));
  }

  // DECODE + ENCODE: the struct parameter is decoded from calldata and the same
  // value re-encoded as the (static, inline) return — returndata == input words.
  function roundtrip(p : Point) public returns (Point) {
    return p;
  }

  // DECODE + field projection: dot access on a decoded struct parameter.
  function getX(p : Point) public returns (uint256) {
    return p.x;
  }

  // DECODE + three projections + construct + ENCODE: returns Point(y, x, color),
  // so the returndata swaps the x/y head words and carries the color field
  // through unchanged.
  function swap(p : Point) public returns (Point) {
    return Point(p.y, p.x, p.color);
  }
}
