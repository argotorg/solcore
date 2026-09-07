enum Nat { Zero, Succ(Nat) }

function natInd (step : function((Nat, Nat)) internal returns (Nat), v : Nat, n : Nat) returns (Nat) {
  match (n ) {
  case Nat.Zero { return v ;
  } case Nat.Succ(m) { return step(m, natInd(step,v,m));
  } }
}

function add(n : Nat, m : Nat) returns (Nat) {
  return natInd (lam (x, acc) {return Nat.Succ(acc) ; }, m, n);
}
