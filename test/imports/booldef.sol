export { Bool(*), not, C, D, id };

enum Bool { True, False }

function not (b : Bool) returns (Bool) {
  match (b ) {
  case Bool.True { return Bool.False;
  } case Bool.False { return Bool.True;
  } }
}

trait C<a> {
  function c (x : a, y : a) returns (word) ;
}

trait D<a> {
  function d() returns (a) ;
}

function id<a> (x : a) returns (word)  where a: C, a: D {
  return C.c(x, D.d());
}
