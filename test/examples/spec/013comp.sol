contract Compose {
  function compose(f,g) public {
    return lam (x) {
      return f(g(x));
    } ;
  }

  function id(x) public { return x; }

  function idid() public { return compose(id,id); }

  function main() public {
    let f = compose(id,id);
    return f(42);
  }
}