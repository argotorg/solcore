// Yul has no boolean type: `true`/`false` are word literals (1/0). A literal
// `true` in an assembly block must type-check as `word`. Before the fix
// `tcYLit YulTrue/YulFalse` called `notImplemented`, crashing the compiler.
contract Test {
  public function main() -> word {
    let r : word = 0;
    assembly {
      let x := true
      r := x
    }
    return r;
  }
}
