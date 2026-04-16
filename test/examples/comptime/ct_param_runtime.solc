/* Negative: non-comptime function parameter passed to a comptime parameter.
   Caught by the SAIL-level check: 'process' CAN be called with an argument
   not known at compile time, which would violate the comptime requirement
   of 'double'.  The SAIL check rejects this on the parameter type alone,
   before looking at specific call sites.
*/
import std;

contract ComptimeParamRuntime {
  function double(comptime x : word) -> comptime word {
    return x + x;
  }
  function process(value : word) -> word {
    return double(value);
  }
  function main() -> word {
    return process(21);
  }
}
