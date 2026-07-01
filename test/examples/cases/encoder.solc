data TagA = TagA(word);
data TagB = TagB(word);

forall self rep.
class self:Tag(rep) {
    function getTag(x:self) -> rep;
}

data TypeA = TypeA(word);
instance TypeA:Tag(TagA) {
    function getTag(x:TypeA) -> TagA {
        match x { | TypeA(w) => return TagA(w); }
    }
}

data TypeB = TypeB(word);
instance TypeB:Tag(TagB) {
    function getTag(x:TypeB) -> TagB {
        match x { | TypeB(w) => return TagB(w); }
    }
}

forall a b rep1 rep2 . a:Tag(rep1), b:Tag(rep2) =>
function tagFirst(x:a, y:b) -> rep1 {
    return Tag.getTag(x);
}

contract C {
    constructor() {}

    public function main() -> word {
        let r : TagA = tagFirst(TypeA(42), TypeB(7));
        match r { | TagA(w) => return w; }
    }
}
