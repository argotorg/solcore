contract Compose {
  function id(x : word) public returns (word) { return x; }

  function idid(x : word) public returns (word) { return id(id(x)); }

  function main() public returns (word) {
    return idid(42);
  }
}
