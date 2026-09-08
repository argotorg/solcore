function constApplied(x : word, y : word) returns (word) {
  return y;
}

contract Foo {
  function main () public returns (word) {
    return constApplied(0,1);
  }
}
