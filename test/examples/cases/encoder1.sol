import {*} from std;

trait Encoder<self, rep> {
    function encode(x:self, hint:word) returns (rep);
}

enum Foo { Foo(word) }
impl Encoder<Foo, word> {
    function encode(x:Foo, hint:word) returns (word) {
        match (x ) { case Foo(w) { return w; } }
    }
}

function encodeAndDiscard<a, rep>(x:a) returns (())  where a: Encoder<rep> {
    let enc : rep = Encoder.encode(x, 0);
    return;
}

contract C {
    function main() public returns (word) {
        encodeAndDiscard(Foo(42));
        return 0;
    }
}
