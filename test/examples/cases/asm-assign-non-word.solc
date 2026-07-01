// An assembly assignment writes a raw scalar word, so its LHS must have type
// 'word'. Assigning to a non-word local (here a 'bool', whose runtime layout
// is a tagged inl/inr pair) would corrupt that layout, so the type checker
// must reject this program.
contract AsmBool {
  public function main() -> word {
    let b : bool = false;
    assembly { b := add(1, 1) }
    if b { return 1; } else { return 0; }
  }
}
