// Returns a constant function that closes over its argument.
// Correct annotations: (word) -> word, body returns the captured word.
function constFn(x : word) returns (function(word) internal returns (word)) {
  return lam (y : word) returns (word) {
    return x;
  };
}
