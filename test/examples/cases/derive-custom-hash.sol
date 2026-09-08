import * from std;
import * from std.Generic;

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

trait Hash<a> {
    function hash(x : a) returns (word);
}

impl Hash<word> {
    function hash(x : word) returns (word) { return x; }
}

impl Hash<()> {
    function hash(x : ()) returns (word) { return 0; }
}

impl<f, g> Hash<sum<f, g>> where f: Hash, g: Hash {
    function hash(x : sum<f, g>) returns (word) {
        match (x ) {
        case inl(u) { return Hash.hash(u);
        } case inr(v) { return Hash.hash(v) + 1;
        } }
    }
}

impl<f, g> Hash<(f, g)> where f: Hash, g: Hash {
    function hash(x : (f, g)) returns (word) {
        match (x ) {
        case (u, v) { return Hash.hash(u) * 31 + Hash.hash(v);
        } }
    }
}

#[derive(Hash)]
enum Color { Red, Green, Blue }

#[derive(Hash)]
enum Pair<a, b> { Pair(a, b) }

function hashRed() returns (word) {
    return Hash.hash(Color.Red);
}

function hashPair() returns (word) {
    let p : Pair<word, word> = Pair(3, 7);
    return Hash.hash(p);
}
