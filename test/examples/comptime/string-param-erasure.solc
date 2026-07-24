// Comptime-only parameters are erased by substituting their literal argument.
//
// A source `Str` instance that *materializes* its argument only works if the
// literal reaches the instance body: inside `fromString` the argument is a
// parameter, so `memStringFromLit(s)` would never match EmitHull's intercept.
// MastEval clones the callee per literal, substitutes it, and drops the
// parameter, so the body ends up holding a `StrLit` again.
//
// Both call sites below go through that path; the second one only works
// because the clone happens after comptime folding, so `concatLit` has
// already collapsed to a single literal by then.
//
// main returns strlen("abcd") + strlen("abcd") = 8.

import std;
import std.{*};

data Wrapped = Wrapped(memory(string));

instance Wrapped : Str {
    function fromString(s: string) -> Wrapped {
        return Wrapped(Str.fromString(s));
    }
}

function unwrap(w: Wrapped) -> memory(string) {
    match w {
      | Wrapped(m) => return m;
    }
}

contract StringParamErasure {
  function main() -> word {
     let direct : Wrapped = "abcd";
     let folded : Wrapped = concatLit("ab", "cd");
     return strlen(unwrap(direct)) + strlen(unwrap(folded));
  }
}
