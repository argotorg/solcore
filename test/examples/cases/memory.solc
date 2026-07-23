enum Memory<t> { Memory(word) }
enum Bytes { Bytes }

function get_bytes() returns (Memory<Bytes>) {
  let ptr : word;
  assembly {
    ptr := mload(0x40)
  }
  return Memory(ptr);
}
