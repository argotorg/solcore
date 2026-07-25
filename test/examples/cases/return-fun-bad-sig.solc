// INCORRECT: signature says the result consumes a bool ((bool) -> word),
// but the returned lambda consumes a word.
function makeF(x : word) returns (function(bool) internal returns (word)) {
  return lam (y : word) returns (word) {
    return x;
  };
}
