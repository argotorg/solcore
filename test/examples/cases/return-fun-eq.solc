// Returns a function comparing against a captured word, CORRECT annotations.
// Uses an assembly `eq` instead of primEqWord so it lowers end-to-end.
function makeEq(x : word) -> ((word) -> word) {
  return lam (y : word) -> word {
    let res : word;
    assembly {
      res := eq(x, y)
    }
    return res;
  };
}

contract C {
  public function main() -> word {
    let f = makeEq(7);
    return f(7);
  }
}
