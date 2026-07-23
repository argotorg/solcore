/* Positive: comptime through an overloaded (type class) function.
   Scale.scale takes a comptime factor; if factor == 1 it returns x
   unchanged (conditional evaluated at comptime since factor is comptime).
   mulWord is builtinPure, so multiplication of comptime values is comptime.
   The verifier must follow specialization and accept this.
*/
import {*} from std;

trait Scale<a> {
  function scale(comptime factor : word, comptime x : a) returns (comptime a);
}

impl Scale<word> {
  function scale(comptime factor : word, comptime x : word) returns (comptime word) {
    if (factor == 1) {
      return x;
    } else {
      return x * factor;
    }
  }
}

contract ComptimeOverloadedOk {
  function main() returns (word) {
    let comptime a :  word = Scale.scale(1, 32);
    let comptime b :  word = Scale.scale(3, 10);
    return a + b;
  }
}
