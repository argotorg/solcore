// A user-defined instance of the primitive `Str` class.
//
// `Str` has two bodyless primitive instances (`string`, `memory(string)`) that
// Specialise rewrites directly.  Any other instance head is ordinary source
// code with a body, so `Str.fromString` at that result type must resolve
// through the normal resolution table rather than falling back to identity.
//
// Here `Tag`'s instance measures the literal, so the whole conversion folds at
// comptime: main returns strlen("abcd") = 4.

import std;
import std.{*};

data Tag = Tag(word);

instance Tag : Str {
    function fromString(s: string) -> Tag {
        return Tag(strlenLit(s));
    }
}

function tagLength(t: Tag) -> word {
    match t {
      | Tag(n) => return n;
    }
}

contract StringUserInstance {
  function main() -> word {
     let t : Tag = "abcd";
     return tagLength(t);
  }
}
