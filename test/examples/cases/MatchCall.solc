data Bool = False | True;

contract MatchCall {
  public function f() -> Bool {
    return Bool.True;
  }

  public function main() -> word {
    match f() {
     | Bool.True => return 42;
     | Bool.False => return 0;
    }
  }
}
