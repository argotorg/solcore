contract RGB {
  enum Color { R, G, B }

  function fromEnum(c : Color) public returns (word) {
    match (c ) {
      case Color.R { return 4;
      } case Color.G { return 2;
      } case Color.B { return 42;
    } }
  }

  function main() public returns (word) { return fromEnum(Color.B); }
}
