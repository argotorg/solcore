import std;
import std.{*};
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

// These functions are intended to be folded by MastEval at compile time.

contract StringLitOps {
  public function main() -> () {
    // concatLit folds to a string literal, enabling revertLit("...") lowering
    let s : comptime string  = concatLit("ab", "cd");
    std.revertLit(s);
  }
}
