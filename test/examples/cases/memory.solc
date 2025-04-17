data Memory(t) = Memory(word);
data Bytes = Bytes;

function get_bytes() -> Memory(Bytes) {
  let ptr : word;
  assembly {
    ptr := mload(0x40)
  }
  return Memory(ptr);
}
