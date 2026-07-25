// INCORRECT: the signature promises a one-argument function (word) -> word,
// but the returned lambda takes two arguments.
function makeF(x : word) returns (function(word) internal returns (word)) {
  return lam (y : word, z : word) returns (word) {
    let res : word;
    assembly {
      res := add(y, z)
    }
    return res;
  };
}
