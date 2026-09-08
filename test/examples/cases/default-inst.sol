trait Test<self> { function f(x:self); }

default impl Test<a> { function f(x:self) {}}

enum memory<a> { memory(word) }
enum Proxy<a> { Proxy }

impl Test<memory<memory<word>>> { function f(x:self) {}}

function f<a>(p:Proxy<a>) {
    let x:memory<a>;
    Test.f(x);
}

function g() {
    f(@memory<memory<word>>); // needs to choose default instance in Test.f
    f(@memory<word>); // needs to choose concrete instance
}
