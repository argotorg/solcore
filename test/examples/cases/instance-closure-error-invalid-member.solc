trait CtFun<t> {
    function ct(x : t) returns (function(t) internal returns (t));
}

impl CtFun<word> {
    function ct(x : word) returns (function(word) internal returns (word)) {
        return lam(y : bool) {
            return x;
        };
    }
}
