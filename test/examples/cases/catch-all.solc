data Bool = False | True;

contract CatchAll {
    public function catchAll(x : Bool, y : Bool) -> Bool{
      match x, y {
      | Bool.True, Bool.True => return Bool.True;
      | z, w      => return z;
      }
    }

  public function main() -> Bool {
    catchAll(Bool.True, Bool.False)
  }
}
