function foo(x : word) -> word {
  return bar(x);
}
function bar(x : word) -> word {
  return foo(x);
}

contract C {
  public function m(x : word) -> word {
    return n(x);
  }
  public function n(x : word) -> word {
    return m(x);
  }
  public function main() -> word {
    return m(1);
  }
}
