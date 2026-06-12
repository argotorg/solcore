// mstore does not return a value, so it cannot be assigned.
contract Test {
  public function main() {
    let x : word;
    assembly {
      x := mstore(1, 1)
    }
    return x;
  }
}
