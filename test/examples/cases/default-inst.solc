trait Test<self> { function f(x:self); }

default impl Test<a> { function f(x:self) {}}

enum memory<a> { memory(word) }
enum Proxy<a> { Proxy }

impl Test<word memory memory> { function f(x:self) {}}

function f<a>(p:Proxy<a>) {
    let x:a memory;
    Test.f(x);
}

function g() {
    f(Proxy as Proxy<word memory memory>); // needs to choose default instance in Test.f
    f(Proxy as Proxy<word memory>); // needs to choose concrete instance
}
