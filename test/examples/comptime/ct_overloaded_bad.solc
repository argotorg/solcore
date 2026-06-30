/* Negative: Scale instance whose 'scale' reads from storage — not comptime.
   Despite the comptime annotations on the method signature, the word
   instance body uses sload (mutable storage state), making the result
   a runtime value.  The verifier must reject the comptime let binding.
*/
import std;

forall a. class a : Scale {
  function scale(comptime factor : word, comptime x : a) -> comptime a;
}

instance word : Scale {
  function scale(comptime factor : word, comptime x : word) -> comptime word {
    let base : word;
    assembly {
      base := sload(0)
    }
    return base + x * factor;
  }
}

contract ComptimeOverloadedBad {
  function main() -> word {
    let a : comptime word = Scale.scale(3, 10);
    return a;
  }
}
