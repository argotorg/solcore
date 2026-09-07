enum Nat { Zero, Succ(Nat) }

function foo (x : Nat, y : Nat) returns (word) {
  match (y, x ) {
  case (y1, Nat.Zero ) { return 1 ;
  } case (Nat.Zero, Nat.Succ(x2) ) { return 2;
  } case (Nat.Succ(y3), Nat.Succ(x3) ) { return 3;
  } }
}

