// Polymorphic literal inference: the type of an unannotated integer literal is
// determined by unification with the surrounding context.
//
//   Add.add(s, 1) with s:word  => 1 infers as word (Add a => a->a->a, a=word)
//   integerAdd(n, 1) with n:integer => 1 infers as integer (param type is integer)

import std.{Add};

contract PolyLit {
  function main() -> word {
    let s : word = 0;
    // 1 inferred as word via Add.add constraint
    let s2 : word = Add.add(s, 1);
    // literal in integer context; type and comptime inferred
    let n = wordToInteger(s2);
    let n2 = integerAdd(n, 1);
    return wordFromInteger(n2);
  }
}
