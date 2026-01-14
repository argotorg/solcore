type MyPair(a, b) = pair(a, b);
type IntPair = MyPair(word, word);

function makePair(x: word, y: word) -> MyPair(word, word) {
    return pair(x, y);
}

function main() -> word {
    let p: IntPair = makePair(42, 100);
    match p {
        | pair(x, _) => return x;
    }
}
