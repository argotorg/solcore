function g(x:word) returns (word) { return x; }

function h<a>(x:a) returns (a) { return x; }

contract C {
  function main() public returns (word) { return g(h(42)); }
}
