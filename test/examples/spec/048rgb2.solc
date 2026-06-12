contract RGB {
  data Color = R | G | B;

  public function fromEnum(c : Color) -> word {
    match c {
      | Color.R => return 4;
      | Color.G => return 2;
      | Color.B => return 42;
    }
  }

  public function main() -> word { return fromEnum(Color.B); }
}
