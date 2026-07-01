class a:Enum {
    function fromEnum(x:a) -> Word;
}

  data Color = R | G | B;

instance Color : Enum {
  function fromEnum(c : Color) -> Word {
    match c {
      | Color.R => return 1;
      | Color.G => return 2;
      | Color.B => return 3;
    }
  }
}


data Bool = False | True;

instance Bool : Enum {
  function fromEnum(b : Bool) -> Word {
      match b {
      | Bool.False => return 0;
      | Bool.True => return 1;
      }
  }
}

contract GoodInstance {
  public function main() -> Word { return fromEnum(Bool.True);}
}
