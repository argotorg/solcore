pragma no-patterson-condition A;

data Wrap(a) = Wrap(a);

forall a . Wrap(a):A => class a:A {}

forall a . Wrap(a):A => function needsWrappedA(x:a) -> () {
  return ();
}

forall a . a:A => function shouldUseSuperclass(x:a) -> () {
  return needsWrappedA(x);
}

function main() -> () {
  return ();
}
