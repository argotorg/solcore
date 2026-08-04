import std.{*};

// erc7201 is comptime-only: given a string-literal namespace it folds the two
// nested keccaks and the word arithmetic down to a single bytes32 slot
// constant, with no runtime hashing.
contract Erc7201Lit {
  public function main() -> bytes32 {
    // keccak256(abi.encode(uint256(keccak256("example.main")) - 1)) & ~0xff
    // == 0x183a6125c38840424c4a85fa12bab2ab606c4b6d0e7cc73c0c06ba5300eab500
    return erc7201("example.main");
  }

  // keccakWordLit on its own: keccak of a word's 32-byte big-endian form.
  public function wordHash() -> word {
    return keccakWordLit(0);
  }
}
