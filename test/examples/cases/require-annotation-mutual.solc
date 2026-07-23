// Error: mutually recursive free functions without annotations
function foo(x : word) {
  return bar(x);
}

function bar(x : word) returns (word) {
  return foo(x);
}
