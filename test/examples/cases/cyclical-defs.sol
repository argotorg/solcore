function foo(x : word) returns (word) {
  return bar(x);
}
function bar(x : word) returns (word) {
  return foo(x);
}

contract C {
  function m(x : word) public returns (word) {
    return n(x);
  }
  function n(x : word) public returns (word) {
    return m(x);
  }
  function main() public returns (word) {
    return m(1);
  }
}
