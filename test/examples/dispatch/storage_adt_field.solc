import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
import std.StorageGeneric.{*};

// Algebraic data types used directly as contract storage fields, including a
// nested ADT (Option(Triple)).
//
//  - someValue  : Option(uint256)  (sum,            rep sum((), uint256)        -> 2 slots)
//  - triple     : Triple           (product,        rep (uint256,(uint256,uint256)) -> 3 slots)
//  - someTriple : Option(Triple)   (sum of product, rep sum((), Triple)         -> 4 slots)

data Option(a) = None | Some(a);
data Triple = Triple(uint256, uint256, uint256);

contract C {
    someValue : Option(uint256);
    triple : Triple;
    someTriple : Option(Triple);

    constructor() {
        // sum:            1 tag + max(size (), size uint256) = 1 + 1 = 2
        assert(StorageSize.size(Proxy : Proxy(Option(uint256))) == 2);
        // product:        size uint256 * 3                   = 3
        assert(StorageSize.size(Proxy : Proxy(Triple)) == 3);
        // sum of product: 1 tag + max(size (), size Triple)  = 1 + 3 = 4
        assert(StorageSize.size(Proxy : Proxy(Option(Triple))) == 4);
    }

    public function setValue(v : uint256) -> () {
        someValue = Option.Some(v);
    }

    public function clearValue() -> () {
        someValue = Option.None;
    }

    public function getValue() -> uint256 {
        match someValue {
        | Option.None    => revertEmpty(); return uint256(0);
        | Option.Some(v) => return v;
        }
    }

    public function isSome() -> bool {
        match someValue {
        | Option.None    => return false;
        | Option.Some(_) => return true;
        }
    }

    public function setTriple(a : uint256, b : uint256, c : uint256) -> () {
        triple = Triple(a, b, c);
    }

    public function tripleSum() -> uint256 {
        match triple {
        | Triple(a, b, c) => return a + b + c;
        }
    }

    // Nested ADT: Option(Triple).
    public function setSomeTriple(a : uint256, b : uint256, c : uint256) -> () {
        someTriple = Option.Some(Triple(a, b, c));
    }

    public function clearSomeTriple() -> () {
        someTriple = Option.None;
    }

    public function someTripleSum() -> uint256 {
        match someTriple {
        | Option.None    => revertEmpty(); return uint256(0);
        | Option.Some(t) =>
            match t {
            | Triple(a, b, c) => return a + b + c;
            }
        }
    }

    public function hasSomeTriple() -> bool {
        match someTriple {
        | Option.None    => return false;
        | Option.Some(_) => return true;
        }
    }
}
