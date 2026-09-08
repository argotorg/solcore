// Runtime test: `#[derive(Eq, Ord)]` on data types declared INSIDE a contract.
// The derived instances are top-level, but the types stay contract-local; each
// public function returns uint256(1) for true / uint256(0) for false.
//   - Color (a contract-local enum) exercises the () and sum(f, g) instances;
//   - Point (a contract-local product) exercises the pair (f, g) instance.

import * from std;
import * from std.dispatch;
import * from std.Generic;

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

contract DeriveContractLocal {
    #[derive(Eq, Ord)]
    enum Color { Red, Green, Blue }

    #[derive(Eq, Ord)]
    enum Point { Point(uint256, uint256) }

    constructor() {}

    // enum equality (reaches the () and sum universe instances)
    function eqRedRed() public returns (uint256) {
        match (Eq.eq(Color.Red, Color.Red) ) {
        case true  { return uint256(1);
        } case false { return uint256(0);
        } }
    }

    function eqRedBlue() public returns (uint256) {
        match (Eq.eq(Color.Red, Color.Blue) ) {
        case true  { return uint256(1);
        } case false { return uint256(0);
        } }
    }

    // enum ordering follows declaration order: Red < Green < Blue
    function gtGreenRed() public returns (uint256) {
        match (Ord.gt(Color.Green, Color.Red) ) {
        case true  { return uint256(1);
        } case false { return uint256(0);
        } }
    }

    function gtRedGreen() public returns (uint256) {
        match (Ord.gt(Color.Red, Color.Green) ) {
        case true  { return uint256(1);
        } case false { return uint256(0);
        } }
    }

    // product equality (reaches the pair universe instance)
    function eqPointSame() public returns (uint256) {
        match (Eq.eq(Point(uint256(1), uint256(2)), Point(uint256(1), uint256(2))) ) {
        case true  { return uint256(1);
        } case false { return uint256(0);
        } }
    }

    // product ordering is lexicographic: the second field breaks the tie
    function gtPointLex() public returns (uint256) {
        match (Ord.gt(Point(uint256(1), uint256(100)), Point(uint256(1), uint256(50))) ) {
        case true  { return uint256(1);
        } case false { return uint256(0);
        } }
    }
}
