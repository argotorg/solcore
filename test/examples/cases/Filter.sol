enum List<a> { Nil, Cons(a, List<a>) }
enum Bool { False, True }

function and(x : Bool, y : Bool) returns (Bool) {
  match (x, y ) {
  case (Bool.False, _ ) { return Bool.False;
  } case (Bool.True, z ) { return z;
  } }
}

trait Eq<a> {
  function eq (x : a, y : a) returns (Bool) ;
}

impl Eq<Word> {
  function eq (x : Word, y : Word) returns (Bool) {
    match (primEqWord(x,y) ) {
    case 0 { return Bool.False ;
    } default { return Bool.True ;
    } }
  }
}


function filter (f : function(Word) internal returns (Bool), xs : List<Word>) returns (List<Word>) {
  match (xs ) {
  case List.Nil { return List.Nil ;
  } case List.Cons(y,ys) {
    match (f(y) ) {
    case Bool.False { return filter(f,ys);
    } case Bool.True { return List.Cons(y,filter(f,ys));
    } }
  } }
}

function list1 () returns (List<Word>) {
  return List.Cons(1, List.Cons(2, List.Cons(3, List.Nil)));
}

function foo0(y : Word) returns (List<Word>) {
  return filter((lam (x){ return eq(x,y); }), list1());
}

function foo1() returns (List<Word>) {
  return filter((lam (x){ return eq(x,1); }), list1());
}

function foo2(p : function(Word) internal returns (Bool), q : function(Word) internal returns (Bool)) returns (List<Word>) {
  return filter(lam (x) { return and(p(x), q(x)) ; }
                , list1());
}

