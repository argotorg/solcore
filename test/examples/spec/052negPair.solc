
trait Neg<a> {
   function neg(x:a) returns (a);
}

enum B { F, T }
enum Pair<a, b> { Pair(a, b) }

impl Neg<B> {
  function neg (x : B) {
    match (x ) {
    case B.F { return B.T;
    } case B.T { return B.F;
    } }
  }
}

function fst (p) {
  match (p ) {
    case Pair(x,y) { return x;
  } }
}

function snd(p) {
  match (p ) {
    case Pair(x,y) { return y;
  } }
}


impl Neg<Pair<a, b>> where a: Neg, b: Neg {
  function neg(p) {
    return Pair(Neg.neg (fst(p)), Neg.neg(snd (p)));
  }
}

/*
instance (a:Neg,b:Neg) => Pair(a,b):Neg {
  function neg(p) {
    match p {
      | Pair(a,b) => return Pair(neg(a), neg(b));
    }
  }
}
*/
contract NegPair {

 function bnot(x) public {
   match (x ) {
     case B.T { return B.F;
     } case B.F { return B.T;
   } }
}

 function fromB(b) public {
  match (b  ) {
    case B.F { return 0;
    } case B.T { return 1;
  } }
}

 function main() public { return  fromB(fst(Neg.neg(Pair(B.F,B.T)))); }
}
