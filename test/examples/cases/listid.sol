enum List<a> { Nil, Cons(a, List<a>) }

function id<a>(x : a) returns (a) {
  return x;
}

function listid(xs : List<word>) returns (List<word>) {
  match (xs ) {
  case List.Nil { return List.Nil ;
  } case List.Cons(x,xs) { return List.Cons(id(x), listid(xs));
  } }
}
