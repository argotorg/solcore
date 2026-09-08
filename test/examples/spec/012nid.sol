contract Id1 {
  function id(x) public {
    return x ;
  }

  function nid() public {
    return id;
  }

  function const(x, y) public { return x; }

  function main() public {
    return const(nid(42), id(1));
  }
}
