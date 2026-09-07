enum Proxy<a> { Proxy }

trait BaseMemoryType<self> {
    function memorySize(x:Proxy<self>) returns (word);
}


function morefun<t>(p:Proxy<t>) returns (word)  where t: BaseMemoryType {
  return BaseMemoryType.memorySize(Proxy as Proxy<t>);
}
