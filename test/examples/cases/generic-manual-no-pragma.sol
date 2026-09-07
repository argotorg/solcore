// Error case: manual Generic instance without pragma no-generic-instance-for.
// The compiler must reject this with a conflict error.

import {*} from std.Generic;

pragma solcore noPattersonCondition;
pragma solcore noBoundVariableCondition;

enum Foo { MkFoo(word) }

impl Generic<Foo, word> {
    function from(x : Foo) returns (word) {
        match (x ) { case Foo.MkFoo(v) { return v; } }
    }
    function to(v : word) returns (Foo) {
        return Foo.MkFoo(v);
    }
}
