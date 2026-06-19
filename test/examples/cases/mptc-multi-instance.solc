// Tests that tryResolveMPTC selects the correct instance when multiple instances
// of the same class are registered in the resolution table.
// For getTag(Foo(1)): specmgu (Bar -> RepBar) (Foo -> freshV) fails (Bar != Foo),
// so only the Foo entry fires and rep is resolved to RepFoo.
// Similarly for getTag(Bar(2)) rep resolves to RepBar.

data Foo = Foo(word);
data Bar = Bar(word);
data RepFoo = RepFoo(word);
data RepBar = RepBar(word);

forall self rep.
class self:Tagged(rep) {
    function tag(x:self) -> rep;
}

instance Foo:Tagged(RepFoo) {
    function tag(x:Foo) -> RepFoo {
        match x { | Foo(w) => return RepFoo(w); }
    }
}

instance Bar:Tagged(RepBar) {
    function tag(x:Bar) -> RepBar {
        match x { | Bar(w) => return RepBar(w); }
    }
}

forall a rep . a:Tagged(rep) =>
function getTag(x:a) -> rep {
    return Tagged.tag(x);
}

contract C {
    constructor() {}
    public function main() -> word {
        let rf : RepFoo = getTag(Foo(1));
        let rb : RepBar = getTag(Bar(2));
        match rf { | RepFoo(w) => return w; }
    }
}
