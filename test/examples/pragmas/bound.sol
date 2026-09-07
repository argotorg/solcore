
trait D<a> { function f(x:a); }
trait F<a, b> {}

enum Memory<a> { Memory(word) }

impl<a> F<Memory<a>, Memory<Memory<Memory<a>>>> {}
impl<a, c> D<Memory<Memory<Memory<a>>>> where c: D, a: F<c> {
    function f(x:Memory<Memory<Memory<a>>>) {}
}

function g<b>(y:b) {
    let x : Memory<Memory<Memory<Memory<b>>>>;
    f(x);
}
