
forall a. function fromWord(x: word) -> a {
      let result : a;
      assembly { result := x } 
      return result;
  }

contract Unsafe {
  public function main() {
    fromWord(7):();
    return 42;
  }
}
