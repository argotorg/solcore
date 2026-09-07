contract Id1 {

  enum Bool { False, True }

  function id(x : word) public returns (word) {
    return x ;
  }

  function const(x : word, y : Bool) public returns (word) { return x; }

  function main() public returns (word) {
    return const(id(42), Bool.False);
  }
}
