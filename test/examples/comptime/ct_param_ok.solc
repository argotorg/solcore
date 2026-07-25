/* Positive: literal passed to comptime param.
   x+x desugars to Add.add(x,x) -> addWord(x,x), which is builtinPure,
   so the comptime annotation on the result is valid.
*/
import std;

contract ComptimeParamOk {
  function double(comptime x : word) returns (comptime word) {
    return x + x;
  }
  function main() returns (word) {
    return double(21);
  }
}
