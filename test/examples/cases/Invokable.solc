
trait invokable<self, args, ret> {
    function invoke (s:self,  a:args) returns (ret);
  }

  function id<a>(x : a) returns (a) {
    return x ;
  }

  enum IdToken<a> { IdToken }

impl invokable<IdToken<a>, a, a> {
  function invoke(token: IdToken<a>, a) returns (a) {
    return id(a);
  }
}
