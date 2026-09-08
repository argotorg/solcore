import * from std;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;
  function zero () returns (word) {
    return 0;
  }

function one() returns (word) {
    return 1 + zero() ;
  }

function two () returns (word) {
  let x = zero();
  x = x + one();
  x =  x + x ;
  return x;
}

contract Plus {
  function main() public returns (word) { return two() + two(); }
}
