contract Id1 {
  public function id(x : word) -> word {
    return x ;
  }

  public function nid(x : word) -> word {
    return id(x);
  }

  public function const(x : word, y : word) -> word { return x; }

  public function main() -> word {
    return const(nid(42), id(1));
  }
}
