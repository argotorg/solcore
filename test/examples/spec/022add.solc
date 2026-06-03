function add(x : word, y : word) -> word {
  let res: word;
  assembly {
     res := add(x, y)
  }
  return res;
}

contract Add1 {
  public function main() -> word {
    return add(40, 2);
  }
}
