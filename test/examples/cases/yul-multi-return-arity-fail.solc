// The arity check must still reject a genuine mismatch: 'pair' returns 2
// values but 3 names are being assigned, so this Yul is invalid and the type
// checker must report the arity error.
contract YulMultiRetBad {
  public function main() -> word {
    let x : word;
    let y : word;
    let z : word;
    assembly {
      function pair() -> a, b {
        a := 1
        b := 2
      }
      x, y, z := pair()
    }
    return x;
  }
}
