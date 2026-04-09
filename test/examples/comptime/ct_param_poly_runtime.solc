/* Negative: comptime violation in a polymorphic (generic) function.
   Before specialisation the concrete type of 'z' is unknown, so this
   cannot be resolved by inlining.  The SAIL-level check catches the
   violation: 'z' is a non-comptime parameter and cannot satisfy the
   comptime contract of 'unwrap'.
*/
import std;

forall t. class t : Wrap {
  function unwrap(comptime x : t) -> comptime word;
}

instance word : Wrap {
  function unwrap(comptime x : word) -> comptime word {
    return x;
  }
}

forall t. t:Wrap => function process(z : t) -> word {
  return Wrap.unwrap(z);
}

contract ComptimeParamPolyRuntime {
  function main() -> word {
    return process(42);
  }
}
