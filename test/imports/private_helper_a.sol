export { foo };

function helper(x: word) returns (word) {
  return x;
}

function foo(x: word) returns (word) {
  return helper(x);
}
