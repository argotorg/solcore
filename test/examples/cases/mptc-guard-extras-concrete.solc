// Tests the guard in resolveMPTCFromPreds that skips tryResolveMPTC when all
// extras are already fully concrete.  Here rep is written as the concrete type
// `word` directly in the constraint, so freetv extras = [] and the function
// compiles through normal type inference without phantom variable discovery.

data Box = Box(word);

forall self rep.
class self:Unbox(rep) {
    function unbox(x:self) -> rep;
}

instance Box:Unbox(word) {
    function unbox(x:Box) -> word {
        match x { | Box(w) => return w; }
    }
}

forall a . a:Unbox(word) =>
function extractWord(x:a) -> word {
    return Unbox.unbox(x);
}

contract C {
    constructor() {}
    public function main() -> word {
        return extractWord(Box(42));
    }
}
