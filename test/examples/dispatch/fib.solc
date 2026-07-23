import {*} from std.dispatch;

function fib(n : word) returns (word) {
   if(n < 2) { return n; } else {return fib(n-1) + fib(n-2); }
}

contract Fib {
  constructor() {}
  function test() public returns (uint256) {
    return uint256(fib(10));
  }
}
