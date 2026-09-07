import * from std;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

function notAnswer(n : word) returns (word) { return ((n == 42) ? 0 : 42 );}

function answer(n:word) returns (word) { return notAnswer(notAnswer(42)); }

contract Fib {
  function main() public returns (word) { return answer(42); }
}
