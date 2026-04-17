data Nat = Zero | Succ(Nat);

function foo(n : Nat) -> Nat {
  match n {
  | Nat.Zero => return Nat.Succ(Nat.Zero) ;
  | Nat.Succ(Nat.Succ(x)) => return x;
  | x => return Nat.Zero;
  }
}
