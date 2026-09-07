enum Pair<a, b> { Pair(a, b) }

function foo<a>(p: a) returns (word) {
    let x: word = p;
    return x;
}
