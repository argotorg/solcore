// Tests that both Template A and Template B fire when the class has methods in
// both directions.  Both should discover the same binding rep=word; the second
// application is idempotent (extSpSubst with the same binding is a no-op).

enum Box { Box(word) }

trait Convert<self, rep> {
    function toRep(x:self) returns (rep);
    function fromRep(x:rep) returns (self);
}

impl Convert<Box, word> {
    function toRep(x:Box) returns (word) {
        match (x ) { case Box(w) { return w; } }
    }
    function fromRep(x:word) returns (Box) {
        return Box(x);
    }
}

function roundtrip<a, rep>(x:a) returns (a)  where a: Convert<rep> {
    let r : rep = Convert.toRep(x);
    return Convert.fromRep(r);
}

contract C {
    constructor() {}
    function main() public returns (word) {
        let b : Box = roundtrip(Box(99));
        match (b ) { case Box(w) { return w; } }
    }
}
