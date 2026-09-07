// Runtime test: `#[derive(Eq, Ord)]` instances executed on the EVM.
// Each public function returns uint256(1) for true / uint256(0) for false,
// pinning down the structural Eq/Ord instances over (), sum and pair:
//   - Color (an enum) exercises the unit () and sum(f, g) instances;
//   - Point (a product) exercises the pair (f, g) instance.

import * from std;
import * from std.dispatch;
import * from std.Generic;

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

#[derive(Eq, Ord)]
enum Color { Red, Green, Blue }

#[derive(Eq, Ord)]
enum Point { Point(uint256, uint256) }

contract DeriveOrd {
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
