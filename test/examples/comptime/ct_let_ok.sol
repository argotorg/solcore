/* Positive: comptime let binding fed from a comptime function call. */
import std;

contract ComptimeLetOk {
  function double(comptime x : word) returns (comptime word) {
    return x + x;
  }
  function main() returns (word) {
    let comptime y :  word = double(21);
    return y;
  }
}
