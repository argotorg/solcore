import * from std;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

function fib(n : word) returns (word) {
   if(n < 2) { return n; } else {return fib(n-1) + fib(n-2); }
}

contract Fib {
function main() public returns (word) {
  return fib(10);
}
}
