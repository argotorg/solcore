enum MemoryWordReader { MemoryWordReader(word) }

function copyToMem(reader:MemoryWordReader, dst:word, cnt: word) returns (()) {
      match (reader ) {
      case MemoryWordReader(ptr) { assembly { mcopy(dst, ptr, cnt) }
      } }
}

contract Main {
  function main() public returns (()) {
    let r : MemoryWordReader = MemoryWordReader(42);
    copyToMem(r, 0, 32);
  }
}
