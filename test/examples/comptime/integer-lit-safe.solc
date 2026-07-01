import std.{*};

// Safety: verify literals pick up the correct type from context, no spurious coercions.
//
//   addWord(1, 2)     — word params, so 1 and 2 get wordFromInteger coercions
//   wordToInteger(42) — word param, so 42 gets wordFromInteger coercion
//   integerEq(wordToInteger(42), wordToInteger(42))
//                     — the 42 literals are inside wordToInteger calls (word param)
//   let z : word = 5  — explicit word annotation, wordFromInteger coercion inserted

contract IntegerLitSafe {
  function main() -> word {
    // word arithmetic: 1 and 2 must stay as word literals
    let a : word = addWord(1, 2);

    // already-explicit coercions: no double-wrapping of the inner 42
    let ok : comptime bool = integerEq(wordToInteger(42), wordToInteger(42));

    // wordFromInteger param is integer, but wordToInteger(10) is a Call not a
    // literal, so no double-wrap; b folds to 10
    let b : comptime word = wordFromInteger(wordToInteger(10));

    // word-annotated let: annotation is word, not integer -> no coercion
    let z : word = 5;

    return addWord(a, addWord(b, z));
  }
}
