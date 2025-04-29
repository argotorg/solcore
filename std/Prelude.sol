class abs:Typedef(rep) {
    function rep(x:abs) -> rep;
    function abs(x:rep) -> abs;
}
data Proxy(a) = Proxy;
data ref(a) = ref(a); // special builtin semantics


class lhs:Assign(rhs) {
    function assign(l:lhs, r:rhs) -> ();
}

forall a.
instance ref(a):Assign(a) {
    function assign(l:ref(a), r:a) -> () {
        // builtin
        return ();
    }
}

forall a.
function deref(x:ref(a)) -> a {
    match x {
        | ref(y) => return y;
    }
}
