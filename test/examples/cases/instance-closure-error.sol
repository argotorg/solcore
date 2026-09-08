trait CtFun<t> {
    function ct(x : t) returns (function(t) returns (t));
}

impl CtFun<word> {
    function ct(x : word) returns (function(word) returns (word)) {
        return lam(y : word) {
            return x;
        };
    }
}
