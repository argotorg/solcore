// Exercises the PARTIAL guard in tryResolveMPTC.
//
// The instance  forall a b. instance Zero:Nth((a,b), a)  has free type variables
// in its extras even after successfully matching Zero against the concrete main type.
// resolveMPTCsFromPreds detects this (concreteExtras still has free vars) and skips
// the instance, letting normal type inference determine the extra type instead.

pragma solcore noCoverageCondition Nth;

enum Zero {}
enum Succ<a> {}
enum Proxy<a> { Proxy }

trait Nth<a, b, c> {
    function nth(x:Proxy<a>, y:b) returns (c);
}

impl<a, b> Nth<Zero, (a, b), a> {
    function nth(x:Proxy<Zero>, y:(a, b)) returns (a) {
        match (y ) { case (a, b) { return a; } }
    }
}

impl<n, a, b, c> Nth<Succ<n>, (a, b), c> where n: Nth<b, c> {
    function nth(x:Proxy<Succ<n>>, y:(a, b)) returns (c) {
        match (y ) { case (a, b) { return Nth.nth(Proxy as Proxy<n>, b); } }
    }
}

contract C {
    constructor() {}
    function main() public returns (word) {
        let p : (word, word, word) = (1, 2, 3);
        let x : word = Nth.nth(Proxy as Proxy<Zero>, p);
        return x;
    }
}
