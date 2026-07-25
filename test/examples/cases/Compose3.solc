contract Compose {
  function id<a>(x : a) public returns (a) { return x; }

  function apply1(f : function(word) internal returns (word), a : word) public returns (word) { return f(a); }

  function idThenId(x : word) public returns (word) { return id(id(x)); }

  function main() public returns (word) {
    return apply1(idThenId, 42);
  }
}
