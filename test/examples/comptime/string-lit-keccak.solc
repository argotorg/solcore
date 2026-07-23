import std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

contract StringLitKeccak {
  function main() public returns (word) {
    // keccakLit folds to a 256-bit word (EVM/Yul semantics)
    return std.keccakLit("abc");
  }
}
