
trait Invokable<self, args, ret> {
    function invoke (s:self,  a:args) returns (ret);
  }

  function id(x) {
    return x ;
  }

  enum IdToken<a> { IdToken }

impl Invokable<IdToken<a>, a, a> {
  function invoke(token: IdToken<a>, arg:a) returns (a) {
    return id(arg);
  }
}

contract InvokeId {
  function id(x) public {
    return x ;
  }

  /*
    function nid() {
    return id;
  }
  */

  function nidimpl() public {
    return IdToken;
  }

  function main() public {
    // Instead of: `return nid(42)`
    return invoke(nidimpl(), 42);
  }
}
