function add(x : word, y : word) returns (word) {
  let res: word;
  assembly {
     res := add(x, y)
  }
  return res;
}

contract Add1 {
  function main() public returns (word) {
    return add(40, 2);
  }
}
