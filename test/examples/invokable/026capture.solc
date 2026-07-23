/* Manual translation of:
contract Id1 {
  function main() {
    let y = 42;
    let nid = lam(x) {return addW(x,y);};
    return nid(17);
  }
}
*/

function addW(x: Word, y:Word) returns (Word) {
   let res : Word;
   assembly {
       res := add(x, y)
    }
    return res;
}

trait Invokable<self, args, ret> {
    function invoke (s:self,  a:args) returns (ret);
}

// env might be a tuple, here it is a single Word
function lam1impl(env: Word, x: c) returns (c) {
   let y = env;
   return addW(x,y);
}

enum Lam1Closure<a> { Lam1Closure(Word) }

impl Invokable<Lam1Closure<a>, a, Word> {
  function invoke(clos: Lam1Closure<a>, arg:a) returns (Word) {
  match (clos ) {
    case Lam1Closure(env) { return lam1impl(env, arg);
    } }
  }
}


contract InvokeCapLam {
function main() public {
  let y = 42;
  let clos = Lam1Closure(y);

  return invoke(clos, 17);
}
}
