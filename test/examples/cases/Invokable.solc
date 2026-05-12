
class self : invokable(args, ret) {
    function invoke (s:self,  a:args) -> ret;
  }

  forall a . function id(x : a) -> a {
    return x ;
  }

  data IdToken(a) = IdToken;

instance IdToken(a) : invokable(a,a) {
  function invoke(token: IdToken(a), a) -> a {
    return id(a);
  }
}
