forall ref deref . class ref:Loadable (deref) {
    function load (r : ref) -> deref;
}

forall ref deref . class ref:Storable (deref) {
    function store (r : ref, d : deref) -> ();
}

// haskell style class constraints
forall ref deref .
    ref : Loadable(deref)
  , ref : Storable(ref) => class ref:Ref (deref) {}
