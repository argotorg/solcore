contract Logic {
  data Bool = True | False;

  public function not (x : Bool) -> Bool {
    match x {
    | Bool.True => return Bool.False ;
    | Bool.False => return Bool.True ;
    }
  }

  public function and(x : Bool, y : Bool) -> Bool {
    match x, y {
    | Bool.False, _ => return Bool.False ;
    | Bool.True , _ => return y ;
    }
  }

  public function and1 (x : Bool, y : Bool) -> Bool {
    match x, y {
    | Bool.False, Bool.False => return Bool.False ;
    | Bool.True , Bool.False => return Bool.False;
    | Bool.False ,Bool.True => return Bool.False;
    | Bool.True, Bool.True => return Bool.True;
    }
  }

  public function elim (f : word, g : word, x : Bool) -> word {
    match x {
    | Bool.True => return f;
    | Bool.False => return g;
    }
  }

  public function main() -> word { return 0; }
}
