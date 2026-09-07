// Test: Generic instances are auto-derived for sum types.
// Neither Option nor Tree has an explicit Generic instance; both should be
// generated automatically by DeriveGeneric.

import {*} from std;
import {*} from std.Generic;

pragma solcore noPattersonCondition;
pragma solcore noBoundVariableCondition;

enum Option<a> { None, Some(a) }

enum Tree<a> { Leaf, Node(Tree<a>, a, Tree<a>) }

// Use the auto-derived instances to check that from/to round-trip.
function roundtripNone() returns (bool) {
    let x : Option<word> = Option.None;
    let r : sum<(), word> = Generic.from(x);
    let x2 : Option<word> = Generic.to(r);
    match (x2 ) {
    case Option.None    { return true;
    } case Option.Some(_) { return false;
    } }
}

function roundtripSome(v : word) returns (bool) {
    let x : Option<word> = Option.Some(v);
    let r : sum<(), word> = Generic.from(x);
    let x2 : Option<word> = Generic.to(r);
    match (x2 ) {
    case Option.None     { return false;
    } case Option.Some(v2) { return eqWord(v, v2);
    } }
}
