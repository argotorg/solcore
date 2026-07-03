pragma no-patterson-condition Wanted;

forall a . class a:Known {}
forall a . class a:Wanted {}

forall a . a:Known => instance a:Wanted {}

forall a . a:Wanted => function needsWanted(x:a) -> () {
  return ();
}

forall a . a:Known => function passKnown(x:a) -> () {
  return needsWanted(x);
}

function main() -> () {
  return ();
}
