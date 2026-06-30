// mstore does not return a value, so it cannot initialize a `let`.
contract Test {
  public function main() {
    assembly {
      let x := mstore(1, 1)
    }
  }
}
