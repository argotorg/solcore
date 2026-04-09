/* Positive: comptime let binding fed from a comptime function call. */
import std;

contract ComptimeLetOk {
  function double(comptime x : word) -> comptime word {
    return x + x;
  }
  function main() -> word {
    let y : comptime word = double(21);
    return y;
  }
}
