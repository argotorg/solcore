contract EvenOdd {
  enum Nat { Zero, Succ(Nat) }
  enum Bool { False, True }

  function even (n : Nat) public returns (Bool) {
    match (n ) {
    case Nat.Zero { return Bool.True;
    } case Nat.Succ(m) { return odd(m);
    } }
  }

  function odd(n : Nat) public returns (Bool) {
    match (n ) {
    case Nat.Zero { return Bool.False;
    } case Nat.Succ(m) { return even(m);
    } }
  }

  function main() public returns (word) { return 0; }
}
