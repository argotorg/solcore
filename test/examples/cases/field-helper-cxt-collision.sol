import * from std;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

enum FooCxt { FooCxt }

contract Foo {
  x: word;

  function get() public returns (word) {
    return x;
  }
}
