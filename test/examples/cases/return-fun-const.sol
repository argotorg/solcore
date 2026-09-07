// Returns a constant function that closes over its argument.
// Correct annotations: (word) -> word, body returns the captured word.
function constFn(x : word) returns (function(word) returns (word)) {
  return lam (y : word) -> word {
    return x;
  };
}
