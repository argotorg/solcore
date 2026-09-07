export { Bool(*), not };

enum Bool { True, False }

function not(b : Bool) returns (Bool) {
  match (b ) {
  case Bool.True { return Bool.False;
  } case Bool.False { return Bool.True;
  } }
}
