alias MyPair<a, b> = pair<a, b>;
alias IntPair = MyPair<word, word>;

function makePair(x: word, y: word) returns (MyPair<word, word>) {
    return pair(x, y);
}

function main() returns (word) {
    let p: IntPair = makePair(42, 100);
    match (p ) {
        case pair(x, _) { return x;
    } }
}
