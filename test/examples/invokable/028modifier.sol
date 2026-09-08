function add1(x) {
  return addW(x,1);
}

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


/* Manual translation of:
contract Id1 {
  // modifier calls its argument and adds one to result
  function add1mod(f) {
    return lam(a) { return add1(f(a)); }
  }

  function foo(x) {
     return addW(x,2);
  }

  function main() {
    let bar = add1mod(foo);
    return bar(39);
  }
}
*/

function foo(x:Word) returns (Word) {
    return addW(x, 2);
}

enum FooToken { FooToken }

impl Invokable<FooToken, Word, Word> {
  function invoke(self:FooToken, arg: Word) returns (Word) {
    return foo(arg);
  }
}

// lambda in add1mod captures a function
// so env contains the closure

function lam1impl<f> (env : f, a:Word)  where f: Invokable<Word, Word> {
  let f = env;
  return add1(invoke(f, a));
}

// we want:
// data Lam1Closure = f:Invokable(Word,Word) => Lam1Closure(f)

enum Lam1Closure<f> { Lam1Closure(f) }

/*
function extractEnv(clos: Lam1Closure(f)) -> f {
  match clos {
    | Lam1Closure(env) => return env;
    };
}
*/
impl Invokable<Lam1Closure<f>, Word, Word> where f: Invokable<Word, Word> {
  function invoke(clos, arg:Word) returns (Word) {
    match (clos ) {
      case Lam1Closure(env) { return lam1impl(env, arg);
    } }
  }
}

function add1mod(f) {
  return Lam1Closure(f);
}

contract Modifier {


function main() public {
  let barClos = add1mod(FooToken);
  return invoke(barClos, 39);
}
}
