import {*} from std;

function fib2(n : word) returns (comptime word) {
   if(n < 2) { return n; } else {return fib2(n-1) + fib2(n-2); }
}

contract Fib {
  function main() returns (word) {
    let comptime res :  word = fib2(10);
    return res;
  }
}
