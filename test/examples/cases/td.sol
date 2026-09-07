trait Typedef<abs, rep> {
    function abs(x:rep) returns (abs);
    function rep(x:abs) returns (rep);
}

/* default */
impl<t> Typedef<t, t> {
    function abs(x:t) returns (t) { return x; }
    function rep(x:t) returns (t) { return x; }
}

function lift1ac<abs, rep, res>(f:function(rep) internal returns (res), x:abs) returns (res)  where abs: Typedef<rep> { return f(Typedef.rep(x)); }


function id<a>(x:a) returns (a) {return x;}
contract TD {
  function main() public returns (word) { return lift1ac(id, 42); }
}
