// Specialiser rejects this program even though the type checker accepts it.
//
// abort_ : word -> a  has a polymorphic return type (it diverges).
// sink_  : b -> word  accepts any argument and discards it.
//
// At the call  sink_(abort_(0))  the intermediate type 'a' (= 'b') is never
// pinned to a concrete type:
//   - The type checker is satisfied because a type 'a' EXISTS that makes the
//     program consistent (any type works); the overall expression has type word.
//   - The specialiser needs a CONCRETE 'a' to emit code for abort_.  It finds
//     no constraint, no instance, and no return-type context to fix 'a', so
//     ensureClosed reports a free type variable and aborts.

forall a.
function abort_(x:word) -> a {
    return abort_(x);
}

forall b.
function sink_(y:b) -> word {
    return 0;
}

contract C {
    constructor() {}
    public function main() -> word {
        return sink_(abort_(0));
    }
}
