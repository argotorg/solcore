// A class-method call in a `return` that is not the last statement of the
// function must take its result type from the declared return type.
//
// The method is resolved by its *result* type alone, so nothing else can pin
// it down.  In tail position this always worked, because the body's type flows
// up to the function level and is unified there; in a non-tail position that
// type is discarded, so the expected type has to reach the call itself.
// Without that, `pick` fails to compile with an ambiguous `a:FromWord`.

data Box(a) = Box(a);

forall a.
class a:FromWord {
  function fromWord(x: word) -> a;
}

instance Box(word):FromWord {
  function fromWord(x: word) -> Box(word) {
    return Box(x);
  }
}

function pick(cond: bool, w: word) -> Box(word) {
  if (cond) { return FromWord.fromWord(w); }
  return Box(w);
}

function unbox(b: Box(word)) -> word {
  match b {
    | Box(x) => return x;
  }
}

contract CallExpectedNonTailReturn {
  function main() -> word {
    return unbox(pick(true, 42));
  }
}
