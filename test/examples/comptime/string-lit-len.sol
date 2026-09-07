import std;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

contract StringLitLen {
  function main() public returns (word) {
    // strlenLit folds to a word
    return std.strlenLit("hello");
  }
}
