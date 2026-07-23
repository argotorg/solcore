contract ListModule {
  enum List<a> { Nil, Cons(a, List<a>) }
  enum Bool { True, False }


  function zipWith<a, b, c> (f : function((a, b)) internal returns (c),xs : List<a>,ys : List<b>) public returns (List<c>) {
    match (xs, ys ) {
    case (List.Nil, List.Nil ) { return List.Nil ;
    } case (List.Cons(x1,xs1), List.Cons(y1,ys1) ) {
      return List.Cons(f(x1,y1), zipWith(f,xs1,ys1)) ;
    } default { return List.Nil;
    } }
  }

  function foldr<a, b>(f : function((a, b)) internal returns (b), v : b, xs : List<a>) public returns (b) {
    match (xs ) {
    case List.Nil { return v;
    } case List.Cons(y,ys) {
      return f(y, foldr(f,v,ys)) ;
    } }
  }

  function main () public returns (word) {
    return 0;
  }
}
