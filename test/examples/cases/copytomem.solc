data MemoryWordReader = MemoryWordReader(word);

function copyToMem(reader:MemoryWordReader, dst:word, cnt: word) -> () {
      match reader {
      | MemoryWordReader(ptr) => assembly { mcopy(dst, ptr, cnt) }
      }
}

contract Main {
  public function main() -> () {
    let r : MemoryWordReader = MemoryWordReader(42);
    copyToMem(r, 0, 32);
  }
}
