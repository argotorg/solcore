trait Loadable<ref, deref> {
    function load (r : ref) returns (deref);
}

function foo<t>(v : t) returns (word)  where t: Loadable<word> {
  return Loadable.load(v);
}
