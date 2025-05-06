class self:Test { function f(x:self); }

default instance a:Test { function f(x:self) {}}

data memory(a) = memory(word);
data Proxy(a) = Proxy;

instance memory(memory(word)):Test { function f(x:self) {}}

forall a.
function f(p:Proxy(a)) {
    let x:memory(a);
    Test.f(x);
}

function g() {
    f(Proxy:Proxy(memory(memory(word)))); // needs to choose default instance in Test.f
    f(Proxy:Proxy(memory(word))); // needs to choose concrete instance
}
