pragma no-patterson-condition A;
pragma no-patterson-condition B;

forall a . class a:A {}
forall a . class a:B {}

forall a . a:B => instance a:A {}
forall a . a:A => instance a:B {}

forall a . a:A => function needsA(x:a) -> () {
  return ();
}

function main() -> () {
  return needsA(0);
}
