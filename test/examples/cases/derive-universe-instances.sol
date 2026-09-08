import * from std;
import * from std.Generic;

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

function eqUnit() returns (bool) {
    let u : () = ();
    return Eq.eq(u, u);
}

function eqInl() returns (bool) {
    let x : sum<word, word> = inl(1);
    let y : sum<word, word> = inl(1);
    return Eq.eq(x, y);
}

function neqTags() returns (bool) {
    let x : sum<word, word> = inl(1);
    let y : sum<word, word> = inr(1);
    return ne(x, y);
}

function ordInlLtInr() returns (bool) {
    let x : sum<word, word> = inl(1);
    let y : sum<word, word> = inr(1);
    return not(Ord.gt(x, y));
}

function eqPair() returns (bool) {
    let p : (word, word) = (1, 2);
    let q : (word, word) = (1, 2);
    return Eq.eq(p, q);
}

function ordPairLex() returns (bool) {
    let p : (word, word) = (1, 100);
    let q : (word, word) = (1, 50);
    return Ord.gt(p, q);
}
