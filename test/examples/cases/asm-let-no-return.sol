// mstore does not return a value, so it cannot initialize a `let`.
contract Test {
  function main() public {
    assembly {
      let x := mstore(1, 1)
    }
  }
}
