data Bool = False | True;

  function second(x : Bool, y : word) -> word {
    match x, y {
    | Bool.True, z => return z;
    | Bool.False, z => return z;
    }
  }

contract Second {
  public function main() -> word {
    second(Bool.True, 42)
  }
}
