
trait Neg<a> {
   function neg(x:a) returns (a);
}

enum B { F, T }

impl Neg<B> {
  function neg (x : B) returns (B) {
    match (x ) {
    case B.F { return B.T;
    } case B.T { return B.F;
    } }
  }
}

function fst<a, b> (p : (a, b)) returns (a) {
  match (p ) {
  case (x,y) { return x;
  } }
}

function snd<a, b>(p : (a, b)) returns (b) {
  match (p ) {
    case (x,y) { return y;
  } }
}


impl<a, b> Neg<(a, b)> where a: Neg, b: Neg {
  function neg(p : (a, b)) returns ((a, b)) {
    return (Neg.neg (fst(p)), Neg.neg(snd (p)));
  }
}

contract NegPair {

 function bnot(x : B) public returns (B) {
   match (x ) {
     case B.T { return B.F;
     } case B.F { return B.T;
   } }
}

 function fromB(b : B) public returns (word) {
  match (b  ) {
    case B.F { return 0;
    } case B.T { return 1;
  } }
}

 function main() public returns (word) { return  fromB(fst(Neg.neg((B.F,B.T)))); }
}
