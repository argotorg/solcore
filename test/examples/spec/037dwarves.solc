contract Dwarves {
  data Dwarf = Doc | Grumpy | Sleepy | Bashful | Happy | Sneezy | Dopey;


  public function fromEnum(c : Dwarf) -> word {
    match c {
      | Dwarf.Doc      => return 1;
      | Dwarf.Grumpy   => return 2;
      | Dwarf.Sleepy   => return 3;
      | Dwarf.Bashful  => return 4;
      | Dwarf.Happy    => return 5;
      | _              => return 0;
    }
  }

  public function main() -> word { return fromEnum(Dwarf.Happy); }
}
