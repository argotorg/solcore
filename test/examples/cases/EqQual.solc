enum Bool { True, False }

trait Eq<a> {
  function eq (x : a, y : a) returns (Bool);
}

trait Ord<a> where a: Eq {
  function lt (x : a, y : a) returns (Bool) ;
}

impl Eq<word> {
  function eq (x : word, y : word) returns (Bool) {
    match (primEqWord(x,y) ) {
    case 0 {
      return Bool.False;
    } default {
      return Bool.True ;
    } }
  }
}

function foo (x : word) returns (Bool) {
  return Eq.eq (x, 0);
}
