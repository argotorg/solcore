contract Not {
  data Bool = False | True;

  public function main() -> word {
    return fromBool(bnot(Bool.False));
  }

  public function fromBool(b : Bool) -> word {
    match(b) {
      | Bool.False => return 0;
      | Bool.True  => return 1;
    }
  }

  public function bnot(b : Bool) -> Bool {
    match b {
      | Bool.False => return Bool.True;
      | Bool.True => return Bool.False;
    }
  }
}
