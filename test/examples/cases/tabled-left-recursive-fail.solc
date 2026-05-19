pragma no-patterson-condition Loop;

forall a . class a:Loop {}

forall a . a:Loop => instance a:Loop {}

forall a . a:Loop => function needsLoop(x:a) -> () {
  return ();
}

function main() -> () {
  return needsLoop(0);
}
