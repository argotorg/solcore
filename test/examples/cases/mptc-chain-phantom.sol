// Tests resolveMPTCsFromPreds in a "chain" scenario:
// - f has phantom rep in its monotype (Foo -> ())
// - inside f, encode returns a value of type rep
// - that value is passed to sink whose monotype is rep -> ()
//
// Without resolveMPTCsFromPreds the SM substitution lacks rep=word when
// sink's specialisation name is being built, which would produce sink$rep
// (wrong) instead of sink$word (correct).

enum Foo { Foo(word) }

trait Encoder<self, rep> {
    function encode(x:self, hint:word) returns (rep);
}

trait Sink<rep, r> {
    function sink(x:rep) returns (());
}

impl Encoder<Foo, word> {
    function encode(x:Foo, hint:word) returns (word) {
        match (x ) { case Foo(v) { return v; } }
    }
}

impl Sink<word, word> {
    function sink(x:word) returns (()) {
        return;
    }
}

// phantom rep: rep does not appear in f's argument or return type.
// Inside the body, encode returns rep and sink consumes rep.
// resolveMPTCsFromPreds must bind rep=word so that sink specialises
// to sink$word (not sink$rep).
function f<a, rep>(x:a) returns (())  where a: Encoder<rep>, rep: Sink<word> {
    let r : rep = Encoder.encode(x, 0);
    Sink.sink(r);
    return;
}

contract C {
    constructor() {}
    function main() public returns (word) {
        f(Foo(42));
        return 0;
    }
}
