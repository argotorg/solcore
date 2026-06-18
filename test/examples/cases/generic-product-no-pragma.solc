import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
import std.ABIGeneric.{*};

pragma no-patterson-condition;
pragma no-coverage-condition;
pragma no-bounded-variable-condition;

data Point = Point(uint256, uint256);

// Manual Generic instance without pragma no-generic-instance-for Point.
// The compiler must reject this with a conflict error.
instance Point : Generic((uint256, uint256)) {
    function from(p : Point) -> (uint256, uint256) {
        match p { | Point(x, y) => return (x, y); }
    }
    function to(t : (uint256, uint256)) -> Point {
        match t { | (x, y) => return Point(x, y); }
    }
}
