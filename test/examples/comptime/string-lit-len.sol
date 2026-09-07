import std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

contract StringLitLen {
  function main() public returns (word) {
    // strlenLit folds to a word
    return std.strlenLit("hello");
  }
}
