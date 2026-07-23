/* Manual translation of:
contract Id1 {

  function foo() {
    let y = 42;
    let nid = lam(x) {return y;};
    return nid;
  }
  function main() {
    return nid(17);
  }
}
*/

trait Invokable<self, args, ret> {
    function invoke (s:self,  a:args) returns (ret);
}

// env might be a tuple, here it is a single Word
function lam1impl(env: Word, x: c) returns (c) { return env; }

enum Lam1Closure<a> { Lam1Closure(Word) }

impl Invokable<Lam1Closure<a>, a, Word> {
  function invoke(clos: Lam1Closure<a>, arg:a) returns (Word) {
  match (clos ) {
    case Lam1Closure(env) { return lam1impl(env, arg);
    } }
  }
}


contract InvokeCapLam {
function foo() public {
  let y = 42;
  let clos = Lam1Closure(y);
  return clos;
}

function main() public {

  return invoke(foo(), 17);
}
}
