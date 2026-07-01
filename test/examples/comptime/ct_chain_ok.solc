/* Positive: comptime result threaded through two comptime functions.
   increment(20) is comptime, so it can be passed to double's comptime param.
*/
import std;

contract ComptimeChainOk {
  function increment(comptime x : word) -> comptime word {
    return x + 1;
  }
  function double(comptime x : word) -> comptime word {
    return x + x;
  }
  function main() -> word {
    return double(increment(20));
  }
}
