enum Bool { False, True }

function not (b : Bool) returns (Bool) {
  match (b ) {
  case Bool.False { return Bool.True ;
  } case Bool.True { return Bool.False ;
  } }
}
