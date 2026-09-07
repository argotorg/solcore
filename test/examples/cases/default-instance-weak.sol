trait Test<self, weak> { function f(x:self) returns (weak); }

enum memory<a> { memory(word) }
enum Proxy<a> { Proxy }
enum Bool { True, False }
default impl Test<a, word> { function f(x:a) returns (word) { return 42; }}

impl Test<memory<memory<word>>, Bool> { function f(x:self) { return Bool.True; }}

// If we choose the default instance to typecheck f,
// this will pass type-checking, since ``r`` is word.
// But: for a = memory(word), ``r`` will be ``bool`` and this is invalid!
function f<a>(p:Proxy<a>) {
    let x:memory<a>;
    let r :word = Test.f(x);
    assembly {
        sstore(0, r)
    }
}

function g() {
    f(@memory<memory<word>>); // valid, since default instance is used
    f(@memory<word>); // PROBLEM: now we have a bool cross the assembly barrier
}
