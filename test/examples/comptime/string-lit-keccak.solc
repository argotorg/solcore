import std;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

contract StringLitKeccak {
  public function main() -> word {
    // keccakLit folds to a 256-bit word (EVM/Yul semantics)
    return std.keccakLit("abc");
  }
}
