type Uint = word;
type Point = pair(word, word);

function useUint(x: Uint) -> word {
    return x;
}

function makePoint(x: word, y: word) -> Point {
    return pair(x, y);
}

function getX(p: Point) -> word {
    match p {
        | pair(x, _) => return x;
    }
}

function main() -> word {
    let p: Point = makePoint(10, 20);
    return getX(p);
}