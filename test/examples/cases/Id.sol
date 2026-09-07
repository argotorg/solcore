function id (x : word) returns (word) {
    return x;
}

contract Id {
  function main () public returns (word) {
    return id(0);
  }
}

