contract Compose {
  public function id(x : word) -> word { return x; }

  public function idid(x : word) -> word { return id(id(x)); }

  public function main() -> word {
    return idid(42);
  }
}
