data Nat = Zero | Succ(Nat) ;

function foo (x : Nat, y : Nat) -> word {
  match y, x {
  | y1, Nat.Zero => return 1 ;
  | Nat.Zero, Nat.Succ(x2) => return 2;
  | Nat.Succ(y3), Nat.Succ(x3) => return 3;
  }
}

