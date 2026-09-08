// INCORRECT: the returned lambda's body has type bool, but the signature
// promises the result is word.
function makeConst(x : word) returns (function(word) returns (word)) {
  return lam (y : word) -> bool {
    return true;
  };
}
