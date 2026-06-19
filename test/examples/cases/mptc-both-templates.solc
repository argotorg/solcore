// Tests that both Template A and Template B fire when the class has methods in
// both directions.  Both should discover the same binding rep=word; the second
// application is idempotent (extSpSubst with the same binding is a no-op).

data Box = Box(word);

forall self rep.
class self:Convert(rep) {
    function toRep(x:self) -> rep;
    function fromRep(x:rep) -> self;
}

instance Box:Convert(word) {
    function toRep(x:Box) -> word {
        match x { | Box(w) => return w; }
    }
    function fromRep(x:word) -> Box {
        return Box(x);
    }
}

forall a rep . a:Convert(rep) =>
function roundtrip(x:a) -> a {
    let r : rep = Convert.toRep(x);
    return Convert.fromRep(r);
}

contract C {
    constructor() {}
    public function main() -> word {
        let b : Box = roundtrip(Box(99));
        match b { | Box(w) => return w; }
    }
}
