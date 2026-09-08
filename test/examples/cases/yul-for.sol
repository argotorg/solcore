contract YulFor {
  function main() public returns (word) {
    let loopStart : word = 128;
    let loopEnd : word = 256;
    let res : word;
    assembly {
      let i := loopStart
      for {} lt(i, loopEnd) { i := add(i, 32) }
      { mstore(i, 42) }
      res := mload(192)
    }
    return res;
  }
}
