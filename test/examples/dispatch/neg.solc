import {*} from std;
import {*} from std.dispatch;

trait Neg<a> {
   function neg(x:a) returns (a);
}

enum B { F, T }
enum Pair<a, b> { Pair(a, b) }

impl Neg<B> {
  function neg (x : B) returns (B) {
    match (x ) {
    case B.F { return B.T;
    } case B.T { return B.F;
    } }
  }
}


function pairfst<a, b> (p : Pair<a, b>) returns (a) {
  match (p ) {
    case Pair(x,y) { return x;
  } }
}

function pairsnd<a, b>(p : Pair<a, b>) returns (b) {
  match (p ) {
    case Pair(x,y) { return y;
  } }
}


impl<a, b> Neg<Pair<a, b>> where a: Neg, b: Neg {
  function neg(p:Pair<a, b>) returns (Pair<a, b>) {
    return Pair(Neg.neg (pairfst(p)), Neg.neg(pairsnd(p)));
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

 function bnot(x:B) returns (B) {
   match (x ) {
     case B.T { return B.F;
     } case B.F { return B.T;
   } }
}

 function fromB(b:B) returns (word) {
  match (b  ) {
    case B.F { return 0;
    } case B.T { return 1;
  } }
}

contract NegPair {
 constructor() {}
 function negPair() public returns (uint256) { return uint256(fromB(pairfst(Neg.neg(Pair(B.F,B.T))))); }
}
