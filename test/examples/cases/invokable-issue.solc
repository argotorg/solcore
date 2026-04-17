forall abs rep . class abs:Typedef(rep) {
    function abs(x:rep) -> abs;
    function rep(x:abs) -> rep;
}

forall t.
/* default */ instance t:Typedef(t) {
    function abs(x:t) -> t { return x; }
    function rep(x:t) -> t { return x; }
}

forall abs rep res. abs:Typedef(rep) =>
function lift1ac(f:(rep) -> res, x:rep) -> res { f(Typedef.rep(x)) }
