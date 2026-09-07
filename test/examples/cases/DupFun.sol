function f(x : word) returns (word) {
  return x;
}

function f(x : word) returns (word) {
  return 10;
}

function g(x : word) returns (word) {
  return f(x);
}
