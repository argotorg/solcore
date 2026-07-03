pragma no-patterson-condition C;

forall a . class a:A {}
forall a . class a:B {}
forall a . class a:C {}

forall a . a:A, a:B => instance a:C {}

forall a . a:C => function needsC(x:a) -> () {
  return ();
}

forall a . a:A, a:B => function fromAB(x:a) -> () {
  return needsC(x);
}

forall a . a:B, a:A => function fromBA(x:a) -> () {
  return needsC(x);
}

function main() -> () {
  return ();
}
