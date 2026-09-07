enum Proxy<a> { Proxy }

trait C<a> {
  function fun(p:Proxy<a>) returns (word);
}

function morefun<t>(p:Proxy<t>) returns (word) {
  return C.fun(Proxy as Proxy<t>);
}
