// Tests the guard in resolveMPTCFromPreds that skips tryResolveMPTC when all
// extras are already fully concrete.  Here rep is written as the concrete type
// `word` directly in the constraint, so freetv extras = [] and the function
// compiles through normal type inference without phantom variable discovery.

enum Box { Box(word) }

trait Unbox<self, rep> {
    function unbox(x:self) returns (rep);
}

impl Unbox<Box, word> {
    function unbox(x:Box) returns (word) {
        match (x ) { case Box(w) { return w; } }
    }
}

function extractWord<a>(x:a) returns (word)  where a: Unbox<word> {
    return Unbox.unbox(x);
}

contract C {
    constructor() {}
    function main() public returns (word) {
        return extractWord(Box(42));
    }
}
