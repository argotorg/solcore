enum List<a> { Nil, Cons(a, List<a>) }
enum Bool { False, True }

function and (x : Bool, y : Bool) returns (Bool) {
  match (x,y ) {
  case (Bool.False, _ ) { return Bool.False;
  } case (Bool.True, y ) { return y;
  } }
}

trait Eq<a> {
  function eq(x : a, y : a) returns (Bool);
}

impl Eq<Bool> {
  function eq (x : Bool, y : Bool) returns (Bool) {
    match (x, y ) {
    case (Bool.False, Bool.False ) { return Bool.True;
    } case (Bool.True, Bool.True ) { return Bool.True;
    } default { return Bool.False;
    } }
  }
}

impl<a> Eq<List<a>> where a: Eq {
  function eq (xs : List<a>, ys : List<a>) returns (Bool) {
    match (xs, ys ) {
    case (List.Nil, List.Nil ) { return Bool.True;
    } case (List.Cons(x,xs), List.Cons(y,ys) ) {
        return and(Eq.eq(x,y),Eq.eq(xs,ys));
    } default { return Bool.False;
    } }
  }
}

function foo() returns (()) {
  let x = Eq.eq(List.Cons(Bool.True,List.Nil), List.Nil);
}
