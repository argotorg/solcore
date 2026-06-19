import std.{*};

forall self rep.
class self:Encoder(rep) {
    function encode(x:self, hint:word) -> rep;
}

data Foo = Foo(word);
instance Foo:Encoder(word) {
    function encode(x:Foo, hint:word) -> word {
        match x { | Foo(w) => return w; }
    }
}

forall a rep . a:Encoder(rep) =>
function encodeAndDiscard(x:a) -> () {
    let enc : rep = Encoder.encode(x, 0);
    return ();
}

contract C {
    public function main() -> word {
        encodeAndDiscard(Foo(42));
        return 0;
    }
}
