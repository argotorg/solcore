import {*} from std;
import {*} from std.dispatch;
import {*} from std.Generic;
import {*} from std.StorageGeneric;

// A recursive data type has no bounded slot footprint, so DeriveGeneric
// (isRecursiveData) deliberately skips deriving StorageSize and
// storage(T):CanStore(T) for it. Using one as a contract field must therefore
// fail: the field's CanStore obligation has no instance.
//
// The failure surfaces at the use site (the field assignment), not at
// derivation time, which is the design stated in DeriveGeneric.

enum IntList { Nil, Cons(uint256, IntList) }

contract C {
    xs : IntList;

    constructor() {
        xs = IntList.Nil;
    }
}
