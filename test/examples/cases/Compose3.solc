contract Compose {
  forall a . public function id(x : a) -> a { return x; }

  public function apply1(f : (word) -> word, a : word) -> word { return f(a); }

  public function idThenId(x : word) -> word { return id(id(x)); }

  public function main() -> word {
    return apply1(idThenId, 42);
  }
}
