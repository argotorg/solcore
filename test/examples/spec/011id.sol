contract Id1 {

  enum Bool { False, True }

  function id(x) public {
    return x ;
  }

  function const(x, y) public { return x; }

  function main() public {
    return const(id(42), Bool.False);
  }
}
