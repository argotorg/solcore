// Runtime test: `#[derive(Eq, Ord)]` instances executed on the EVM.
// Each public function returns uint256(1) for true / uint256(0) for false,
// pinning down the structural Eq/Ord instances over (), sum and pair:
//   - Color (an enum) exercises the unit () and sum(f, g) instances;
//   - Point (a product) exercises the pair (f, g) instance.

import std.{*};
import std.dispatch.{*};
import std.Generic.{*};

pragma no-patterson-condition;
pragma no-bounded-variable-condition;

#[derive(Eq, Ord)]
data Color = Red | Green | Blue;

#[derive(Eq, Ord)]
data Point = Point(uint256, uint256);

contract DeriveOrd {
    constructor() {}

    // enum equality (reaches the () and sum universe instances)
    public function eqRedRed() -> uint256 {
        match Eq.eq(Color.Red, Color.Red) {
        | true  => return uint256(1);
        | false => return uint256(0);
        }
    }

    public function eqRedBlue() -> uint256 {
        match Eq.eq(Color.Red, Color.Blue) {
        | true  => return uint256(1);
        | false => return uint256(0);
        }
    }

    // enum ordering follows declaration order: Red < Green < Blue
    public function gtGreenRed() -> uint256 {
        match Ord.gt(Color.Green, Color.Red) {
        | true  => return uint256(1);
        | false => return uint256(0);
        }
    }

    public function gtRedGreen() -> uint256 {
        match Ord.gt(Color.Red, Color.Green) {
        | true  => return uint256(1);
        | false => return uint256(0);
        }
    }

    // product equality (reaches the pair universe instance)
    public function eqPointSame() -> uint256 {
        match Eq.eq(Point(uint256(1), uint256(2)), Point(uint256(1), uint256(2))) {
        | true  => return uint256(1);
        | false => return uint256(0);
        }
    }

    // product ordering is lexicographic: the second field breaks the tie
    public function gtPointLex() -> uint256 {
        match Ord.gt(Point(uint256(1), uint256(100)), Point(uint256(1), uint256(50))) {
        | true  => return uint256(1);
        | false => return uint256(0);
        }
    }
}
