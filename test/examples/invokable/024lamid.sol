contract Id1 {
  function id(x) public {
    return x ;
  }


  function main() public {
    let nid = lambda(x) {return x;};
    return nid(42);
  }
}