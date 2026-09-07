// INCORRECT: the signature promises a function (word) -> word, but the body
// returns a plain word instead of a function.
function makeF(x : word) returns (function(word) returns (word)) {
  return x;
}
