// Tests tryResolveMPTC Template A path.
// The class has only a method of the form (self -> rep), so Template B cannot
// fire.  The specialiser must discover rep=word solely via Template A:
//   specmgu (Box -> word) (Box -> freshV)  =>  freshV = word  =>  rep = word

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

forall a rep . a:Unbox(rep) =>
function extract(x:a) -> rep {
    return Unbox.unbox(x);
}

contract C {
    constructor() {}
    public function main() -> word {
        return extract(Box(42));
    }
}
