// Tests tryResolveMPTC Template A path.
// The class has only a method of the form (self -> rep), so Template B cannot
// fire.  The specialiser must discover rep=word solely via Template A:
//   specmgu (Box -> word) (Box -> freshV)  =>  freshV = word  =>  rep = word

enum Box { Box(word) }

trait Unbox<self, rep> {
    function unbox(x:self) returns (rep);
}

impl Unbox<Box, word> {
    function unbox(x:Box) returns (word) {
        match (x ) { case Box(w) { return w; } }
    }
}

function extract<a, rep>(x:a) returns (rep)  where a: Unbox<rep> {
    return Unbox.unbox(x);
}

contract C {
    constructor() {}
    function main() public returns (word) {
        return extract(Box(42));
    }
}
