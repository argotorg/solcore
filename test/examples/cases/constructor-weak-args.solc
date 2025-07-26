forall ref deref . class ref:Loadable (deref) {
    function load (r : ref) -> deref;
}

forall t . t : Loadable(word) => function foo(v : t) -> word {
  return Loadable.load(v);
}
