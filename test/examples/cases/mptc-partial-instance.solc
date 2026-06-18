// Exercises the PARTIAL guard in tryResolveMPTC.
//
// The instance  forall a b. instance Zero:Nth((a,b), a)  has free type variables
// in its extras even after successfully matching Zero against the concrete main type.
// resolveMPTCsFromPreds detects this (concreteExtras still has free vars) and skips
// the instance, letting normal type inference determine the extra type instead.

pragma no-coverage-condition Nth;

data Zero;
data Succ(a);
data Proxy(a) = Proxy;

forall a b c. class a:Nth(b, c) {
    function nth(x:Proxy(a), y:b) -> c;
}

forall a b. instance Zero:Nth((a,b), a) {
    function nth(x:Proxy(Zero), y:(a,b)) -> a {
        match y { | (a, b) => return a; }
    }
}

forall n a b c. n:Nth(b,c) => instance Succ(n):Nth((a,b), c) {
    function nth(x:Proxy(Succ(n)), y:(a,b)) -> c {
        match y { | (a, b) => return Nth.nth(Proxy : Proxy(n), b); }
    }
}

contract C {
    constructor() {}
    public function main() -> word {
        let p : (word, word, word) = (1, 2, 3);
        let x : word = Nth.nth(Proxy : Proxy(Zero), p);
        return x;
    }
}
