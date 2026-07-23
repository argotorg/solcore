// Instance member returning a function with CORRECT annotations.
// The compiled-away validation pass used to check this; the single pass must too.
forall t . class t:CtFun {
    function ct(x : t) -> ((t) -> t);
}

instance word:CtFun {
    function ct(x : word) -> ((word) -> word) {
        return lam (y : word) -> word {
            return x;
        };
    }
}
