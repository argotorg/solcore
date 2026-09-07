import * from std;
import * from std.dispatch;
import * from std.Generic;
import * from std.StorageGeneric;

// A Solidity-style struct used directly as a contract storage field.
//
// A `struct` is a single-constructor product, so it stores exactly like the
// `Triple` product in storage_adt_field.sol: each field occupies its own
// StorageSize-many slots (here 2 uint256 words = 2 slots), reached through the
// derived storage(Point):CanStore(Point) instance. The whole struct can be
// written and read back, destructured with `match`, or read field-by-field via
// dot notation (a whole-struct load followed by a field projection).
//
// A single field can also be *written* through dot notation: `p.x = v`
// desugars to a whole-struct read-modify-write `p = set_x(p, v)` (the setter is
// generated per field alongside the projections), reusing the same
// CanStore.store path.

struct Point {
  x: uint256;
  y: uint256;
}

contract C {
  p : Point;

  constructor() {
    // product: size uint256 * 2 = 2 slots
    assert(StorageSize.size(@Point) == 2);
  }

  // whole-struct write
  function setPoint(a : uint256, b : uint256) public returns (()) {
    p = Point(a, b);
  }

  // read back by destructuring with match
  function sumMatch() public returns (uint256) {
    match (p ) {
    case Point(a, b) { return a + b;
    } }
  }

  // read a single field via dot access on the storage struct
  function getX() public returns (uint256) {
    return p.x;
  }

  function getY() public returns (uint256) {
    return p.y;
  }

  // write a single field via dot access (read-modify-write the whole struct)
  function setX(v : uint256) public returns (()) {
    p.x = v;
  }

  function setY(v : uint256) public returns (()) {
    p.y = v;
  }
}
