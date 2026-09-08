// Instance member returning a function with CORRECT annotations.
// The compiled-away validation pass used to check this; the single pass must too.
trait CtFun<t> {
    function ct(x : t) returns (function(t) returns (t));
}

impl CtFun<word> {
    function ct(x : word) returns (function(word) returns (word)) {
        return lam (y : word) -> word {
            return x;
        };
    }
}
