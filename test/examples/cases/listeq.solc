enum List<a> { Nil, Cons(a, List<a>) }
enum Bool { False, True }

trait Eq<a> {
  function eq (x : a, y : a) returns (Bool) ;
}

function foo () {
  return Eq.eq(List.Nil, List.Nil);
}
