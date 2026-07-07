// Returns a function with CORRECT type annotations.
// Validates the single-pass type checker: closure conversion must not hide
// that the returned lambda really has type (word) -> word.
// Uses an assembly block instead of primAddWord so it lowers end-to-end.
function makeAdder(x : word) -> ((word) -> word) {
  return lam (y : word) -> word {
    let res : word;
    assembly {
      res := add(x, y)
    }
    return res;
  };
}

contract C {
  public function main() -> word {
    let f = makeAdder(10);
    return f(5);
  }
}
