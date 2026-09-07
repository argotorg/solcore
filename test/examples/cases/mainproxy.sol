enum Proxy<a> { Proxy }

trait BaseMemoryType<self> {
    function memorySize(x:Proxy<self>) returns (word);
}


impl BaseMemoryType<word> {
    function memorySize(x:Proxy<self>) returns (word) {
      return 32;
    }
}


function morefun(p:Proxy<t>) returns (word) { return BaseMemoryType.memorySize(Proxy as Proxy<t>);
}

contract TestMemoryType {
  function main() public returns (word) {
    return morefun(Proxy as Proxy<word>);
  }
}
