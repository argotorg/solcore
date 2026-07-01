// Test: Generic instances are auto-derived for sum types.
// Neither Option nor Tree has an explicit Generic instance; both should be
// generated automatically by DeriveGeneric.

import std.{*};
import std.Generic.{*};

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

data Option(a) = None | Some(a);

data Tree(a) = Leaf | Node(Tree(a), a, Tree(a));

// Use the auto-derived instances to check that from/to round-trip.
function roundtripNone() -> bool {
    let x : Option(word) = Option.None;
    let r : sum((), word) = Generic.from(x);
    let x2 : Option(word) = Generic.to(r);
    match x2 {
    | Option.None    => return true;
    | Option.Some(_) => return false;
    }
}

function roundtripSome(v : word) -> bool {
    let x : Option(word) = Option.Some(v);
    let r : sum((), word) = Generic.from(x);
    let x2 : Option(word) = Generic.to(r);
    match x2 {
    | Option.None     => return false;
    | Option.Some(v2) => return eqWord(v, v2);
    }
}
