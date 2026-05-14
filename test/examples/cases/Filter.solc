data List(a) = Nil | Cons(a,List(a));
data Bool = False | True;

function and(x : Bool, y : Bool) -> Bool {
  match x, y {
  | Bool.False, _ => return Bool.False;
  | Bool.True, z => return z;
  }
}

class a : Eq {
  function eq (x : a, y : a) -> Bool ;
}

instance Word : Eq {
  function eq (x : Word, y : Word) -> Bool {
    match primEqWord(x,y) {
    | 0 => return Bool.False ;
    | _ => return Bool.True ;
    }
  }
}


function filter (f : (Word) -> Bool, xs : List(Word)) -> List(Word) {
  match xs {
  | List.Nil => return List.Nil ;
  | List.Cons(y,ys) =>
    match f(y) {
    | Bool.False => return filter(f,ys);
    | Bool.True => return List.Cons(y,filter(f,ys));
    }
  }
}

function list1 () -> List(Word) {
  return List.Cons(1, List.Cons(2, List.Cons(3, List.Nil)));
}

function foo0(y : Word) -> List(Word) {
  return filter((lam (x){ return eq(x,y); }), list1());
}

function foo1() -> List(Word) {
  return filter((lam (x){ return eq(x,1); }), list1());
}

function foo2(p : (Word) -> Bool, q : (Word) -> Bool) -> List(Word) {
  return filter(lam (x) { return and(p(x), q(x)) ; }
                , list1());
}

