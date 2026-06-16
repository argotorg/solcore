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

data Foo = Foo(word);

forall self rep.
class self:Encoder(rep) {
    function encode(x:self, hint:word) -> rep;
}

instance Foo:Encoder(word) {
    function encode(x:Foo, hint:word) -> word {
        match x { | Foo(v) => return v; }
    }
}

forall a rep. a:Encoder(rep) =>
function extractVal(x:a) -> rep {
    return Encoder.encode(x, 0);
}

contract C {
    constructor() {}
    public function main() -> word {
        return extractVal(Foo(7));
    }
}
