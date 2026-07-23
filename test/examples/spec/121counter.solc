// test single contract field
import std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

contract Counter {
  counter : word;

  function main() public returns (word) {
    counter = std.addWord(counter, 1);
    return counter;
  }
}
