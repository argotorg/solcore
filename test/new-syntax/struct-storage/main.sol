import * from std;
import * from std.dispatch;
import * from std.Generic;
import * from std.StorageGeneric;

contract StructStorageUpdates {
  struct Point {
    x: uint256;
    y: uint256;
  }
  p: Point;
  xs: array<uint256>;
  ticks: uint256;

  constructor() {}

  function nextIndex() returns(uint256) {
    ticks += 1;
    return 0;
  }

  // Contract-local named products need valid qualified getter/setter and Hull
  // type names. The updates must retain the untouched second field.
  function exercise() public returns((uint256, uint256, uint256, uint256)) {
    p = Point(12, 7);
    p.x *= 3;
    p.x /= 2;
    p.x += 4;
    p.x ~=;
    p.x ~=;
    // Each compound update evaluates its indexing expression exactly once.
    xs = [12];
    ticks = 0;
    xs[nextIndex()] *= 3;
    xs[nextIndex()] /= 3;
    xs[nextIndex()] ~=;
    xs[nextIndex()] ~=;
    // Fields nested in a tuple constructor also undergo storage desugaring.
    return (p.x, p.y, xs[uint256(0)], ticks);
  }
}
