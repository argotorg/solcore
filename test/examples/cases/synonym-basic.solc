alias Uint = word;
alias Point = pair<word, word>;

function useUint(x: Uint) returns (word) {
    return x;
}

function makePoint(x: word, y: word) returns (Point) {
    return pair(x, y);
}

function getX(p: Point) returns (word) {
    match (p ) {
        case pair(x, _) { return x;
    } }
}

function main() returns (word) {
    let p: Point = makePoint(10, 20);
    return getX(p);
}
