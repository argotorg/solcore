contract Logic {
  enum Bool { True, False }

  function not (x : Bool) public returns (Bool) {
    match (x ) {
    case Bool.True { return Bool.False ;
    } case Bool.False { return Bool.True ;
    } }
  }

  function and(x : Bool, y : Bool) public returns (Bool) {
    match (x, y ) {
    case (Bool.False, _ ) { return Bool.False ;
    } case (Bool.True , _ ) { return y ;
    } }
  }

  function and1 (x : Bool, y : Bool) public returns (Bool) {
    match (x, y ) {
    case (Bool.False, Bool.False ) { return Bool.False ;
    } case (Bool.True , Bool.False ) { return Bool.False;
    } case (Bool.False ,Bool.True ) { return Bool.False;
    } case (Bool.True, Bool.True ) { return Bool.True;
    } }
  }

  function elim (f : word, g : word, x : Bool) public returns (word) {
    match (x ) {
    case Bool.True { return f;
    } case Bool.False { return g;
    } }
  }

  function main() public returns (word) { return 0; }
}
