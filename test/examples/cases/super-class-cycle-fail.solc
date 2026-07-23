forall a . a:B => class a:A {}
forall a . a:A => class a:B {}
forall a . class a:C {}

forall a . a:C => function needsC(x:a) -> () {
  return ();
}

forall a . a:A => function cannotGetC(x:a) -> () {
  return needsC(x);
}

function main() -> () {
  return ();
}
