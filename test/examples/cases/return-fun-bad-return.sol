// INCORRECT: the returned lambda's body has type bool, but the signature
// promises the result is word.
function makeConst(x : word) returns (function(word) internal returns (word)) {
  return lam (y : word) returns (bool) {
    return true;
  };
}
