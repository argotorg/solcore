import {*} from std;
import {*} from std.dispatch;
import {*} from std.Generic;
import {*} from std.StorageGeneric;

// Algebraic data types used directly as contract storage fields, including a
// nested ADT (Option(Triple)).
//
//  - someValue  : Option(uint256)  (sum,            rep sum((), uint256)        -> 2 slots)
//  - triple     : Triple           (product,        rep (uint256,(uint256,uint256)) -> 3 slots)
//  - someTriple : Option(Triple)   (sum of product, rep sum((), Triple)         -> 4 slots)

enum Option<a> { None, Some(a) }
enum Triple { Triple(uint256, uint256, uint256) }

contract C {
    someValue : Option<uint256>;
    triple : Triple;
    someTriple : Option<Triple>;

    constructor() {
        // sum:            1 tag + max(size (), size uint256) = 1 + 1 = 2
        assert(StorageSize.size(Proxy as Proxy<Option<uint256>>) == 2);
        // product:        size uint256 * 3                   = 3
        assert(StorageSize.size(Proxy as Proxy<Triple>) == 3);
        // sum of product: 1 tag + max(size (), size Triple)  = 1 + 3 = 4
        assert(StorageSize.size(Proxy as Proxy<Option<Triple>>) == 4);
    }

    function setValue(v : uint256) public returns (()) {
        someValue = Option.Some(v);
    }

    function clearValue() public returns (()) {
        someValue = Option.None;
    }

    function getValue() public returns (uint256) {
        match (someValue ) {
        case Option.None    { revertEmpty(); return uint256(0);
        } case Option.Some(v) { return v;
        } }
    }

    function isSome() public returns (bool) {
        match (someValue ) {
        case Option.None    { return false;
        } case Option.Some(_) { return true;
        } }
    }

    function setTriple(a : uint256, b : uint256, c : uint256) public returns (()) {
        triple = Triple(a, b, c);
    }

    function tripleSum() public returns (uint256) {
        match (triple ) {
        case Triple(a, b, c) { return a + b + c;
        } }
    }

    // Nested ADT: Option(Triple).
    function setSomeTriple(a : uint256, b : uint256, c : uint256) public returns (()) {
        someTriple = Option.Some(Triple(a, b, c));
    }

    function clearSomeTriple() public returns (()) {
        someTriple = Option.None;
    }

    function someTripleSum() public returns (uint256) {
        match (someTriple ) {
        case Option.None    { revertEmpty(); return uint256(0);
        } case Option.Some(t) {
            match (t ) {
            case Triple(a, b, c) { return a + b + c;
            } }
        } }
    }

    function hasSomeTriple() public returns (bool) {
        match (someTriple ) {
        case Option.None    { return false;
        } case Option.Some(_) { return true;
        } }
    }
}
