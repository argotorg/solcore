import std;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

contract Test {
  function main() public returns (word) {
    return std.addWord(21, 21);
  }
}
