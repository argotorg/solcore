contract T {
}

contract C {
  enum T {
    Inner(word)
  }
}

contract QualifiedNestedType {
  function main() {
    let value: C.T = C.T.Inner(7);
  }
}
