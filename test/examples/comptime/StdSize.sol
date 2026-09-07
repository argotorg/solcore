enum Proxy<t> { Proxy }

function addWord(l: word, r: word) returns (word) {
  let rw: word;
  assembly {
    rw := add(l, r)
  }
  return rw;
}

trait StorageSize<self> {
  function size(x: Proxy<self>) returns (word);
}

default impl<self> StorageSize<self> {
  function size(x: Proxy<self>) returns (word) {
    return 1;
  }
}

impl StorageSize<()> {
  function size(x: Proxy<()>) returns (word) {
    return 0;
  }
}

impl StorageSize<word> {
  function size(x: Proxy<word>) returns (word) {
    return 1;
  }
}

impl<a, b> StorageSize<(a, b)> where a: StorageSize, b: StorageSize {
  function size(x: Proxy<(a, b)>) returns (word) {
    let a_sz: word = StorageSize.size(Proxy as Proxy<a>);
    let b_sz: word = StorageSize.size(Proxy as Proxy<b>);
    return addWord(a_sz, b_sz);
  }
}

contract Size {
  function main() public returns (word) {
    return StorageSize.size(Proxy as Proxy<(word, (word, ()))>);
  }
}
