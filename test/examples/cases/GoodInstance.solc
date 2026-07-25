trait Enum<a> {
    function fromEnum(x:a) returns (Word);
}

  enum Color { R, G, B }

impl Enum<Color> {
  function fromEnum(c : Color) returns (Word) {
    match (c ) {
      case Color.R { return 1;
      } case Color.G { return 2;
      } case Color.B { return 3;
    } }
  }
}


enum Bool { False, True }

impl Enum<Bool> {
  function fromEnum(b : Bool) returns (Word) {
      match (b ) {
      case Bool.False { return 0;
      } case Bool.True { return 1;
      } }
  }
}

contract GoodInstance {
  function main() public returns (Word) { return fromEnum(Bool.True);}
}
