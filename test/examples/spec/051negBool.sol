
trait Neg<a> {
   function neg(x:a) returns (a);
}

enum B { F, T }


impl Neg<B> {
  function neg (x : B) {
    match (x ) {
    case B.F { return B.T;
    } case B.T { return B.F;
    } }
  }
}


contract NegBool {

  function fromB(b) public {
    match (b  ) {
    case B.F { return 0;
    } case B.T { return 1;
    } }
  }

  function main() public { return  fromB(Neg.neg(B.F)); }
}
