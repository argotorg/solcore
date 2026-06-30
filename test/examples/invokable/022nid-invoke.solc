
class self : Invokable(args, ret) {
    function invoke (s:self,  a:args) -> ret;
  }

  function id(x) {
    return x ;
  }

  data IdToken(a) = IdToken

instance IdToken(a) : Invokable(a,a) {
  function invoke(token: IdToken(a), arg:a) -> a {
    return id(arg);
  }
}

contract InvokeId {
  public function id(x) {
    return x ;
  }

  /*
    function nid() {
    return id;
  }
  */

  public function nidimpl() {
    return IdToken;
  }

  public function main() {
    // Instead of: `return nid(42)`
    return invoke(nidimpl(), 42);
  }
}
