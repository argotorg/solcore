// INCORRECT: the signature promises a one-argument function (word) -> word,
// but the returned lambda takes two arguments.
function makeF(x : word) -> ((word) -> word) {
  return lam (y : word, z : word) -> word {
    let res : word;
    assembly {
      res := add(y, z)
    }
    return res;
  };
}
