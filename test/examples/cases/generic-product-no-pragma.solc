import {*} from std;
import {*} from std.dispatch;
import {*} from std.Generic;
import {*} from std.ABIGeneric;

pragma solcore noPattersonCondition;
pragma solcore noCoverageCondition;
pragma solcore noBoundVariableCondition;

enum Point { Point(uint256, uint256) }

// Manual Generic instance without pragma no-generic-instance-for Point.
// The compiler must reject this with a conflict error.
impl Generic<Point, (uint256, uint256)> {
    function from(p : Point) returns ((uint256, uint256)) {
        match (p ) { case Point(x, y) { return (x, y); } }
    }
    function to(t : (uint256, uint256)) returns (Point) {
        match (t ) { case (x, y) { return Point(x, y); } }
    }
}
