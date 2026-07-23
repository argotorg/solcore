function compose<a, b, c, d, e>(f : d, g : e) returns (t_closure1<a, b, c, d, e>)  where d: invokable<b, c>, e: invokable<a, b> {
  return t_closure1(f,g);
}

enum t_closure1<a, b, c, d, e> { t_closure1(d, e) }

function lambda2<a, b, c, d, e>(c : t_closure1<a, b, c, d, e>, x : a) returns (c)  where d: invokable<b, c>, e: invokable<a, b> {
    match (c ) {
    case t_closure1(f, g) {
        return invokable.invoke(f, invokable.invoke(g,x));
    } }
  }

impl<a, b, c, d, e> invokable<t_closure1<a, b, c, d, e>, a, c> where d: invokable<b, c>, e: invokable<a, b> {
  function invoke(self : t_closure1<a, b, c, d, e>, args : a) returns (c) {
    return lambda2(self, args);
  }
}

enum t_id3<a> { t_id3 }

function id<a> (x : a) returns (a) {
  return x;
}

impl<a> invokable<t_id3<a>, a, a> {
  function invoke(self : t_id3<a>, args : a) returns (a) {
    match (self ) {
    case t_id3 { return id(args) ;
    } }
  }
}

contract Foo {
  function main() public returns (word) {
    let f = compose(t_id3, t_id3);
    return invokable.invoke(f, 0);
  }
}
