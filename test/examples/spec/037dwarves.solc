contract Dwarves {
  enum Dwarf { Doc, Grumpy, Sleepy, Bashful, Happy, Sneezy, Dopey }


  function fromEnum(c : Dwarf) public returns (word) {
    match (c ) {
      case Dwarf.Doc      { return 1;
      } case Dwarf.Grumpy   { return 2;
      } case Dwarf.Sleepy   { return 3;
      } case Dwarf.Bashful  { return 4;
      } case Dwarf.Happy    { return 5;
      } default { return 0;
    } }
  }

  function main() public returns (word) { return fromEnum(Dwarf.Happy); }
}
