// Test: pragma no-generic-instance-for suppresses auto-derivation for the
// listed types.  Pair has its instance suppressed and provided manually;
// Box gets its instance generated automatically.

import std.{*};
import std.Generic.{*};

pragma no-patterson-condition;
pragma no-bounded-variable-condition;
pragma no-generic-instance-for Pair;

data Pair(a, b) = MkPair(a, b);

data Box(a) = MkBox(a);

// Manual instance for Pair (suppressed from auto-derivation).
forall a b.
instance Pair(a, b) : Generic((a, b)) {
    function from(p : Pair(a, b)) -> (a, b) {
        match p {
        | Pair.MkPair(x, y) => return (x, y);
        }
    }
    function to(t : (a, b)) -> Pair(a, b) {
        match t {
        | (x, y) => return Pair.MkPair(x, y);
        }
    }
}

// Box gets its Generic instance auto-derived (not excluded).
function boxRoundtrip(v : word) -> bool {
    let b : Box(word) = Box.MkBox(v);
    let r : word = Generic.from(b);
    let b2 : Box(word) = Generic.to(r);
    match b2 {
    | Box.MkBox(v2) => return eqWord(v, v2);
    }
}
