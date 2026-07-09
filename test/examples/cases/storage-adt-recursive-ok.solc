import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
import std.StorageGeneric.{*};

// The counterpart of storage-adt-recursive-fail.solc: skipping storage
// derivation for a recursive type is a SKIP, not a hard error. The type still
// gets its Generic instance and remains usable everywhere except storage.

data IntList = Nil | Cons(uint256, IntList);

function len(xs : IntList) -> uint256 {
    match xs {
    | IntList.Nil        => return uint256(0);
    | IntList.Cons(_, r) => return uint256(1) + len(r);
    }
}

// A non-recursive neighbour in the same module still gets its storage
// instances, so the skip is per-type rather than per-module.
data Point = Point(uint256, uint256);

contract C {
    p : Point;

    constructor() {
        p = Point(uint256(1), uint256(2));
        assert(StorageSize.size(Proxy : Proxy(Point)) == 2);
    }
}
