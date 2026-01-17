forall t . class t:CtFun {
    function ct(x : t) -> ((t) -> t);
}

instance word:CtFun {
    function ct(x : word) -> ((word) -> word) {
        return lam(y : word) {
            return x;
        };
    }
}
