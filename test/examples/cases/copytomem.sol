enum MemoryWordReader { MemoryWordReader(word) }

function copyToMem(reader:MemoryWordReader, dst:word, cnt: word) returns (unit) {
      match (reader ) {
      case MemoryWordReader(ptr) { assembly { mcopy(dst, ptr, cnt) }
      } }
}

contract Main {
  function main() public returns (unit) {
    let r : MemoryWordReader = MemoryWordReader(42);
    copyToMem(r, 0, 32);
  }
}
