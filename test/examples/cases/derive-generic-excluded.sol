// Test: pragma no-generic-instance-for suppresses auto-derivation for the
// listed types.  Pair has its instance suppressed and provided manually;
// Box gets its instance generated automatically.

import {*} from std;
import {*} from std.Generic;

pragma solcore noPattersonCondition;
pragma solcore noBoundVariableCondition;
pragma solcore noGenericInstanceFor Pair;

enum Pair<a, b> { MkPair(a, b) }

enum Box<a> { MkBox(a) }

// Manual instance for Pair (suppressed from auto-derivation).
impl<a, b> Generic<Pair<a, b>, (a, b)> {
    function from(p : Pair<a, b>) returns ((a, b)) {
        match (p ) {
        case Pair.MkPair(x, y) { return (x, y);
        } }
    }
    function to(t : (a, b)) returns (Pair<a, b>) {
        match (t ) {
        case (x, y) { return Pair.MkPair(x, y);
        } }
    }
}

// Box gets its Generic instance auto-derived (not excluded).
function boxRoundtrip(v : word) returns (bool) {
    let b : Box<word> = Box.MkBox(v);
    let r : word = Generic.from(b);
    let b2 : Box<word> = Generic.to(r);
    match (b2 ) {
    case Box.MkBox(v2) { return eqWord(v, v2);
    } }
}
