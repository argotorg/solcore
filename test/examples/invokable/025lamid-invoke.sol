/* Manual translation of:
contract Id1 {
  function main() {
    let nid = lam(x) {return x;};
    return nid(42);
  }
}
*/

trait Invokable<self, args, ret> {
    function invoke (s:self,  a:args) returns (ret);
}

function lam0impl(x: c) returns (c) { return x; }

enum Lam0Token<a> { Lam0Token }

impl Invokable<Lam0Token<a>, a, a> {
  function invoke(token: Lam0Token<a>, arg:a) returns (a) {
    return lam0impl(arg);
  }
}


contract InvokeLam {
function main() public {
  let nid = Lam0Token;
  return invoke(nid, 42);
}
}
