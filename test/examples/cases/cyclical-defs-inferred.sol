function foo(x : word) returns (word) {
  return bar(x);
}
function bar(x : word) returns (word) {
  return foo(x);
}

contract C {
  function main() public returns (word) {
    return foo(1);
  }
}
