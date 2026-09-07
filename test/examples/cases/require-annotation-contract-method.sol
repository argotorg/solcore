// Error: contract method missing return type annotation
contract Doubler {
  function double(x : word) public {
    return x;
  }

  function main() public returns (word) {
    return double(21);
  }
}
