// A Yul function with multiple named returns must keep its true return arity:
// 'x, y := pair()' assigns 2 values from a 2-return function and is valid Yul,
// so the type checker must accept it (regression for the arity check that used
// to collapse every non-empty return list to a single 'word').
contract YulMultiRet {
  public function main() -> word {
    let x : word;
    let y : word;
    assembly {
      function pair() -> a, b {
        a := 1
        b := 2
      }
      x, y := pair()
    }
    return x;
  }
}
