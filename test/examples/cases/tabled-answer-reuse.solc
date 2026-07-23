pragma no-patterson-condition Derived;

forall a . class a:Seed {}
forall a . class a:Derived {}

instance word:Seed {}

forall a . a:Seed => instance a:Derived {}

forall a . a:Derived, a:Derived => function needsDerivedTwice(x:a) -> () {
  return ();
}

function main() -> () {
  return needsDerivedTwice(0);
}
