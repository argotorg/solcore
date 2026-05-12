data Nat = Zero | Succ(Nat);

function natInd (step : (Nat, Nat) -> Nat, v : Nat, n : Nat) -> Nat {
  match n {
  | Nat.Zero => return v ;
  | Nat.Succ(m) => return step(m, natInd(step,v,m));
  }
}

function add(n : Nat, m : Nat) -> Nat {
  return natInd (lam (x, acc) {return Nat.Succ(acc) ; }, m, n);
}
