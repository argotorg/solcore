data Bool = False | True;

function test(v0 : Bool, p : Bool) -> Bool {
  match p {
  | Bool.True => return Bool.False;
  | z    => return v0;
  }
}

contract FreshVariableShadowing {
  public function main() -> Bool {
    test(Bool.True, Bool.False)
  }
}
