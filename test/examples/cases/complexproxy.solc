enum Proxy<a> { Proxy }

function add(x:word, y: word) {return x;}

trait BaseMemoryType<self> {
    function memorySize(x:Proxy<self>) returns (word);
}


impl BaseMemoryType<word> {
    function memorySize(x:Proxy<word>) returns (word) {
      return 32;
    }
}

impl<a, b> BaseMemoryType<(a, b)> where a: BaseMemoryType, b: BaseMemoryType {

    function memorySize(x) returns (word) { // not correct semantically, just for debugging
            return add(BaseMemoryType.memorySize(Proxy as Proxy<a>),
            // BaseMemoryType.memorySize(Proxy:Proxy(b))
	    morefun(Proxy as Proxy<b>)
	    );
    }
}
// this should trigger a type error.
function morefun<t>(p:Proxy<t>) returns (word) {
  return BaseMemoryType.memorySize(Proxy as Proxy<t>);
}

contract TestMemoryType {
  function main() public returns (word) {
    return BaseMemoryType.memorySize(Proxy as Proxy<(word, word)>);
  }
}
