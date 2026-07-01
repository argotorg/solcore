contract EvenOdd {
  data Nat = Zero | Succ(Nat);
  data Bool = False | True;

  public function even (n : Nat) -> Bool {
    match n {
    | Nat.Zero => return Bool.True;
    | Nat.Succ(m) => return odd(m);
    }
  }

  public function odd(n : Nat) -> Bool {
    match n {
    | Nat.Zero => return Bool.False;
    | Nat.Succ(m) => return even(m);
    }
  }

  public function main() -> word { return 0; }
}
