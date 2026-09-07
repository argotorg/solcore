import * from std;
import * from std.Generic;

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

#[derive(Eq)]
enum Pair<a, b> { Pair(a, b) }

function samePair() returns (bool) {
    let p : Pair<word, word> = Pair(1, 2);
    let q : Pair<word, word> = Pair(1, 2);
    return Eq.eq(p, q);
}

function diffPair() returns (bool) {
    let p : Pair<word, word> = Pair(1, 2);
    let q : Pair<word, word> = Pair(1, 3);
    return ne(p, q);
}
