function add(x : word, y : word) {
  let res: word;
  assembly {
      res := add(x, y)
  }
  return res;
}

trait Typedef<self, underlyingType> {
    function rep(x:self) returns (underlyingType);
    function abs(x:underlyingType) returns (self);
}

trait Add<a> {
  function add(x:a, y:a) returns (a);
}

enum B { F, T }


impl Typedef<B, word> {
  function rep(x : B) returns (word) {
    match (x ) {
      case B.F { return 0;
      } case B.T { return 1;
    } }
  }

  function abs(x : word) returns (B) {
    match (x ) {
      case 0 { return B.F;
      } case 1 { return B.T;
    } }
  }
}

impl Add<B> {
  function add(x : B, y : B) returns (B) {
    match (x ) {
      case B.F {
        match (y ) {
          case B.F { return B.F;
          } case B.T { return B.T;
        } }

      } case B.T {
        match (y ) {
          case B.F { return B.T;
          } case B.T { return B.F;
        } }
    } }
  }
}

function fun(a : (B, B), b : (B, B)) returns ((B, B)) { //  -> c
  match (a, b ) {
    case ((a1, a2), (b1, b2) ) { return (Add.add(a1, b1), fun(a2, b2));
  } }

}

contract Compose {

  function main() public returns (word) {
    let res = fun ((B.T, B.T, B.F), (B.F, B.F, B.T));
    match (res ) {
      case (r1, r2, r3) { return Typedef.rep(r1);
    } }
  }
}
