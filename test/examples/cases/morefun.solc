data Proxy(a) = Proxy;

forall a . class a:C {
    function fun(p:Proxy(a)) -> word;
}

forall t . t : C => function morefun(p:Proxy(t)) -> word {
    return C.fun(Proxy:Proxy(t));
}
