import std;
import {*} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

// These functions are intended to be folded by MastEval at compile time.

contract StringLitOps {
  function main() public returns (()) {
    // concatLit folds to a string literal, enabling revertLit("...") lowering
    let comptime s :  string  = concatLit("ab", "cd");
    std.revertLit(s);
  }
}
