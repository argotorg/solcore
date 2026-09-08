contract ArrayImplicitRuntime {
  function main() returns (word) {
    let first: word = 10;
    let second: word = 20;
    let values = [first, second];
    return 7;
  }
}
