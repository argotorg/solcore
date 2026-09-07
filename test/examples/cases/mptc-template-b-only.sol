// Tests tryResolveMPTC Template B path.
// The class has only a method of the form (rep -> self), so Template A cannot
// fire.  The specialiser must discover rep=word solely via Template B:
//   specmgu (word -> Box) (freshV -> Box)  =>  freshV = word  =>  rep = word
// The `hint:a` argument makes a=Box concrete at the call site.

enum Box { Box(word) }

trait Rebox<self, rep> {
    function rebox(x:rep) returns (self);
}

impl Rebox<Box, word> {
    function rebox(x:word) returns (Box) {
        return Box(x);
    }
}

function rewrap<a, rep>(val:rep, hint:a) returns (a)  where a: Rebox<rep> {
    return Rebox.rebox(val);
}

contract C {
    constructor() {}
    function main() public returns (word) {
        let b : Box = rewrap(7, Box(0));
        match (b ) { case Box(w) { return w; } }
    }
}
