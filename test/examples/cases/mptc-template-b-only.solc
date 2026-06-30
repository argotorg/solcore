// Tests tryResolveMPTC Template B path.
// The class has only a method of the form (rep -> self), so Template A cannot
// fire.  The specialiser must discover rep=word solely via Template B:
//   specmgu (word -> Box) (freshV -> Box)  =>  freshV = word  =>  rep = word
// The `hint:a` argument makes a=Box concrete at the call site.

data Box = Box(word);

forall self rep.
class self:Rebox(rep) {
    function rebox(x:rep) -> self;
}

instance Box:Rebox(word) {
    function rebox(x:word) -> Box {
        return Box(x);
    }
}

forall a rep . a:Rebox(rep) =>
function rewrap(val:rep, hint:a) -> a {
    return Rebox.rebox(val);
}

contract C {
    constructor() {}
    public function main() -> word {
        let b : Box = rewrap(7, Box(0));
        match b { | Box(w) => return w; }
    }
}
