forall a . a:B => class a:A {}
forall a . a:A => class a:B {}

forall a . a:B => function needsB(x:a) -> () {
  return ();
}

forall a . a:A => function usesSuperCycle(x:a) -> () {
  return needsB(x);
}

function main() -> () {
  return ();
}
