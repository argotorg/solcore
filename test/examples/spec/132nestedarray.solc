// Nested storage arrays: `array(array(uint256))` with `grid[i][j]` used as both
// an l-value and an r-value. The inner index desugars as an l-value, yielding the
// `storage(array(uint256))` handle that the outer index then consumes.
import {*} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

contract NestedArray {
  reserved : word; // forge uses at least 1 storage slot
  grid : uint256[][];

  function main() returns (uint256) {
    // Grow the outer array; the inner arrays start empty.
    Array.setLength(grid, uint256(2));

    // `grid[i]` yields the inner array's handle, which push can grow.
    ArrayPush.push(grid[uint256(0)], uint256(5));
    ArrayPush.push(grid[uint256(1)], uint256(7));

    // Indexed write, then indexed read.
    grid[uint256(1)][uint256(0)] = uint256(9);

    return grid[uint256(0)][uint256(0)] + grid[uint256(1)][uint256(0)];
  }
}
