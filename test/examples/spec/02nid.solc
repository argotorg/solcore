contract Id1 {
  function id(x : word) public returns (word) {
    return x ;
  }

  function nid(x : word) public returns (word) {
    return id(x);
  }

  function const(x : word, y : word) public returns (word) { return x; }

  function main() public returns (word) {
    return const(nid(42), id(1));
  }
}
