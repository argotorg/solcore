data Proxy(a) = Proxy;

forall self . class self:BaseMemoryType {
    function memorySize(x:Proxy(self)) -> word;
}


forall t . t : BaseMemoryType =>
function morefun(p:Proxy(t)) -> word {
  return BaseMemoryType.memorySize(Proxy:Proxy(t));
}
