// INCORRECT: the returned lambda's parameter is `bool`, but the signature
// promises (word) -> word.  Closure conversion would erase the arrow type;
// the single-pass checker must still reject this.
function makeAdder(x : word) -> ((word) -> word) {
  return lam (y : bool) -> word {
    return x;
  };
}
