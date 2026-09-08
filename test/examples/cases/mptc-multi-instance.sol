// Tests that tryResolveMPTC selects the correct instance when multiple instances
// of the same class are registered in the resolution table.
// For getTag(Foo(1)): specmgu (Bar -> RepBar) (Foo -> freshV) fails (Bar != Foo),
// so only the Foo entry fires and rep is resolved to RepFoo.
// Similarly for getTag(Bar(2)) rep resolves to RepBar.

enum Foo { Foo(word) }
enum Bar { Bar(word) }
enum RepFoo { RepFoo(word) }
enum RepBar { RepBar(word) }

trait Tagged<self, rep> {
    function tag(x:self) returns (rep);
}

impl Tagged<Foo, RepFoo> {
    function tag(x:Foo) returns (RepFoo) {
        match (x ) { case Foo(w) { return RepFoo(w); } }
    }
}

impl Tagged<Bar, RepBar> {
    function tag(x:Bar) returns (RepBar) {
        match (x ) { case Bar(w) { return RepBar(w); } }
    }
}

function getTag<a, rep>(x:a) returns (rep)  where a: Tagged<rep> {
    return Tagged.tag(x);
}

contract C {
    constructor() {}
    function main() public returns (word) {
        let rf : RepFoo = getTag(Foo(1));
        let rb : RepBar = getTag(Bar(2));
        match (rf ) { case RepFoo(w) { return w; } }
    }
}
