import {*} from std;

function fib3(n : word) returns (word) {
   if(n < 2) { return n; } else {return fib3(n-1) + fib3(n-2); }
}

contract Fib {
  function main() returns (word) {
    let comptime res :  word = fib3(10);
    return res;
  }
}
