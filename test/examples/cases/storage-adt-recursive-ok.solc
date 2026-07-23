import {*} from std;
import {*} from std.dispatch;
import {*} from std.Generic;
import {*} from std.StorageGeneric;

// The counterpart of storage-adt-recursive-fail.solc: skipping storage
// derivation for a recursive type is a SKIP, not a hard error. The type still
// gets its Generic instance and remains usable everywhere except storage.

enum IntList { Nil, Cons(uint256, IntList) }

function len(xs : IntList) returns (uint256) {
    match (xs ) {
    case IntList.Nil        { return uint256(0);
    } case IntList.Cons(_, r) { return uint256(1) + len(r);
    } }
}

// A non-recursive neighbour in the same module still gets its storage
// instances, so the skip is per-type rather than per-module.
enum Point { Point(uint256, uint256) }

contract C {
    p : Point;

    constructor() {
        p = Point(uint256(1), uint256(2));
        assert(StorageSize.size(Proxy as Proxy<Point>) == 2);
    }
}
