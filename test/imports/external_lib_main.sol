import @extlib.math.api;

contract External {
  constructor() {}

  function main() public returns (word) {
    return math.api.sum(39);
  }
}
