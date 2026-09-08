// Documents the NOP-A guard in resolveMPTCsFromPreds.
//
// The guard `null (freetv mainTy')` is false when the main type variable
// is not yet bound in the SM substitution.  This happens for higher-order
// polymorphic functions that are specialised from the outside.
//
// Here `mapEncode` is only ever called with a concrete `a=Foo`, so at every
// call site the SM substitution has a=Foo before the body is processed.
// However, if `mapEncode` were called with an unresolved type the guard
// would fire and tryResolveMPTC would be skipped.
//
// This is a compile-only test: it verifies that the NOP-A guard does NOT
// interfere with the normal specialisation of `mapEncode` when called
// from a concrete call site.

enum Foo { Foo(word) }

trait Encoder<self, rep> {
    function encode(x:self, hint:word) returns (rep);
}

impl Encoder<Foo, word> {
    function encode(x:Foo, hint:word) returns (word) {
        match (x ) { case Foo(v) { return v; } }
    }
}

function extractVal<a, rep>(x:a) returns (rep)  where a: Encoder<rep> {
    return Encoder.encode(x, 0);
}

contract C {
    constructor() {}
    function main() public returns (word) {
        return extractVal(Foo(7));
    }
}
