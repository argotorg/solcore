// An uninitialized Yul `let x` must introduce the binding so that later
// assignments and reads of `x` resolve and are type-checked as `word`.
// Before the fix `tcYulStmt` dropped `YLet ns Nothing`, so `x` never entered
// the env and the read `r := x` failed to resolve.
contract Test {
  public function main() -> word {
    let r : word = 0;
    assembly {
      let x
      x := add(1, 1)
      r := x
    }
    return r;
  }
}
