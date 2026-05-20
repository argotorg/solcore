// Error: mutually recursive free functions without annotations
function foo(x : word) {
  return bar(x);
}

function bar(x : word) -> word {
  return foo(x);
}
