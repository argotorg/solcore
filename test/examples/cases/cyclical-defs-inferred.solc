function foo(x : word) -> word {
  return bar(x);
}
function bar(x : word) -> word {
  return foo(x);
}

contract C {
  public function main() -> word {
    return foo(1);
  }
}
