
data memory(t) = memory(word);

forall t . class t:ValueTy {
    function rep(x:t) -> word;
}

forall t . instance memory(t) : ValueTy {
    function rep(x: memory(t)) -> word {
        match x {
        | memory(w) => return w;
       }
    }
}

forall ref deref . class ref:Ref(deref) {
    function store(loc: ref, value: deref) -> ();
}

forall t . t : ValueTy => instance memory(t) : Ref(t) {
    function store(loc: memory(t), value: t) -> () {
        // We don't have a `ValueTy` bound on `t` anywhere, so this should raise a type error...
        let vw = ValueTy.rep(value);
    }
}
