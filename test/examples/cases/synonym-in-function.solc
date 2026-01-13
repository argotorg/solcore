// Synonyms in function parameter and return types
type Int = word;
type Point = pair(Int, Int);

function add(a: Int, b: Int) -> Int {
    return a;
}

function makePoint(x: Int, y: Int) -> Point {
    return pair(x, y);
}

function getX(p: Point) -> Int {
    match p {
        | pair(x, _) => return x;
    }
}

function getY(p: Point) -> Int {
    match p {
        | pair(_, y) => return y;
    }
}

function main() -> word {
    let a: Int = 10;
    let b: Int = 20;
    let p: Point = makePoint(a, b);
    return getX(p);
}