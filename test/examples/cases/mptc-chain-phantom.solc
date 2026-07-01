// Tests resolveMPTCsFromPreds in a "chain" scenario:
// - f has phantom rep in its monotype (Foo -> ())
// - inside f, encode returns a value of type rep
// - that value is passed to sink whose monotype is rep -> ()
//
// Without resolveMPTCsFromPreds the SM substitution lacks rep=word when
// sink's specialisation name is being built, which would produce sink$rep
// (wrong) instead of sink$word (correct).

data Foo = Foo(word);

forall self rep.
class self:Encoder(rep) {
    function encode(x:self, hint:word) -> rep;
}

forall rep r.
class rep:Sink(r) {
    function sink(x:rep) -> ();
}

instance Foo:Encoder(word) {
    function encode(x:Foo, hint:word) -> word {
        match x { | Foo(v) => return v; }
    }
}

instance word:Sink(word) {
    function sink(x:word) -> () {
        return ();
    }
}

// phantom rep: rep does not appear in f's argument or return type.
// Inside the body, encode returns rep and sink consumes rep.
// resolveMPTCsFromPreds must bind rep=word so that sink specialises
// to sink$word (not sink$rep).
forall a rep . a:Encoder(rep), rep:Sink(word) =>
function f(x:a) -> () {
    let r : rep = Encoder.encode(x, 0);
    Sink.sink(r);
    return ();
}

contract C {
    constructor() {}
    public function main() -> word {
        f(Foo(42));
        return 0;
    }
}
