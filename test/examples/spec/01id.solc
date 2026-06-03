contract Id1 {

  data Bool = False | True;

  public function id(x : word) -> word {
    return x ;
  }

  public function const(x : word, y : Bool) -> word { return x; }

  public function main() -> word {
    return const(id(42), Bool.False);
  }
}
