/* Negative: comptime violation in a polymorphic (generic) function.
   Before specialisation the concrete type of 'z' is unknown, so this
   cannot be resolved by inlining.  The SAIL-level check catches the
   violation: 'z' is a non-comptime parameter and cannot satisfy the
   comptime contract of 'unwrap'.
*/
import std;

trait Wrap<t> {
  function unwrap(comptime x : t) returns (comptime word);
}

impl Wrap<word> {
  function unwrap(comptime x : word) returns (comptime word) {
    return x;
  }
}

function process<t>(z : t) returns (word)  where t: Wrap {
  return Wrap.unwrap(z);
}

contract ComptimeParamPolyRuntime {
  function main() returns (word) {
    return process(42);
  }
}
