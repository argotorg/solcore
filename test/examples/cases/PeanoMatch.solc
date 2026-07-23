enum Nat { Zero, Succ(Nat) }

function foo(n : Nat) returns (Nat) {
  match (n ) {
  case Nat.Zero { return Nat.Succ(Nat.Zero) ;
  } case Nat.Succ(Nat.Succ(x)) { return x;
  } case x { return Nat.Zero;
  } }
}
